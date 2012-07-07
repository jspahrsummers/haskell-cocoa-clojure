module ObjcCodeGen (codegen, codegenToFile)
    where

import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Char
import Data.List
import System.IO
import Util

codegen :: [A.Form] -> IO String
codegen forms = do
    let st = GeneratorState { counter = 0 }
    blocks <- evalStateT (codegenMain forms) st

    return $ showDelimList "\n" (sort blocks)

codegenToFile :: Handle -> [A.Form] -> IO ()
codegenToFile fd forms = do
    str <- codegen forms
    hPutStrLn fd str

codegenMain :: [A.Form] -> GeneratorStateT [BasicBlock]
codegenMain forms = do
    (stmts, blocks) <- runWriterT $ genForms forms

    let imports = [SystemImport "Foundation/Foundation.h", SystemImport "objc/runtime.h"]
        mainType = FunctionType IntType [IntType, PointerType (PointerType CharType)]
        mainProto = FuncProto {
            funcType = mainType,
            funcName = (Identifier "main"),
            funcParams = map Identifier ["argc", "argv"]
        }
    
        retStmt = Return $ IntLiteral 0

    return $ imports ++ (FuncDef mainProto (stmts ++ [retStmt]) : blocks)

genForms :: [A.Form] -> BlockGeneratorT [Statement]
genForms [] = return []
genForms (form : rest) = do
    (finalExpr, stmts) <- runWriterT $ genForm form
    s2 <- genForms rest

    -- Make sure to emit the final expression, even if the value is not used
    return $ stmts ++ [Statement finalExpr] ++ s2

-- Emits code for a form
-- Returns an expression representing the value of the form
genForm :: A.Form -> StatementGeneratorT Expr
genForm A.EmptyForm = return VoidExpr

genForm (A.StringLiteral s) = genUniqueInitDecl $ NSStringLiteral s
genForm (A.BooleanLiteral b) = genUniqueInitDecl $ ToObjExpr $ BoolLiteral b
genForm (A.CharacterLiteral c) = genUniqueInitDecl $ NSStringLiteral [c]

-- TODO: this should use something like EXTNil, so it can be put into collections
genForm A.NilLiteral = genUniqueInitDecl NilLiteral

-- TODO: handle numbers bigger than an int
genForm (A.IntegerLiteral n) = genUniqueInitDecl $ ToObjExpr $ IntLiteral $ fromInteger n

-- TODO: handle BigDecimals
genForm (A.DecimalLiteral n) = genUniqueInitDecl $ ToObjExpr $ DoubleLiteral n

genForm (A.Vector forms) = do
    exprs <- mapM genForm forms
    genUniqueInitDecl $ NSArrayLiteral exprs

genForm (A.Map kvs) = do
    let (keys, values) = unzip kvs

    keyExprs <- mapM genForm keys
    valueExprs <- mapM genForm values

    genUniqueInitDecl $ NSDictionaryLiteral $ zip keyExprs valueExprs

genForm (A.Set forms) = do
    exprs <- mapM genForm forms

    let rec = IdentExpr $ Identifier "NSSet"
        sel = Selector "setWithObjects:"

    genUniqueDecl (InstanceType $ Identifier "NSSet") $ VarargMessageExpr rec sel [] exprs

genForm (A.Symbol s) =
    -- Symbols in Clojure and selectors in Objective-C are both kinds of interned strings
    -- The Objective-C runtime is already set up to index selectors in a global table,
    -- and symbols might represent selectors anyways, so let's take advantage of it
    let sel = Selector s
        parts = filter (not . null) $ splitOn ':' s
    in genUniqueDecl SelectorType $
        -- If this symbol is legal for an @selector() expression...
        if (length parts == 1 || last s == ':' ) && (foldl (&&) True $ map isLegalIdentifier parts)
            -- Then use @selector()
            then SelectorLiteral sel
            -- Otherwise, cheat by doing it at runtime
            else CallExpr (IdentExpr $ Identifier "NSStringFromSelector") [NSStringLiteral s]

-- TODO: generate code for an empty list
--genForm (A.List [])

-- TODO: this probably isn't the right way to do this
genForm (A.List ((A.Symbol s):forms)) = do
    exprs <- mapM genForm forms

    -- TODO: handle symbols that aren't valid identifiers
    return $ CallExpr (IdentExpr $ Identifier s) exprs

genForm (A.List forms) = do
    exprs <- mapM genForm forms
    return $ CallExpr (head exprs) (tail exprs)

{-
genForm (A.RationalLiteral n) =
-}

-- Generates a unique variable declaration with the given type and initializer
-- Returns a IdentExpr to use the variable
genUniqueDecl :: Type -> Expr -> StatementGeneratorT Expr
genUniqueDecl t expr = do
    id <- lift $ lift uniqueId

    let var = Identifier $ "v" ++ (show id)
    tell $ [Declaration t var expr]

    return $ IdentExpr var

genUniqueInitDecl :: Expr -> StatementGeneratorT Expr
genUniqueInitDecl expr = genUniqueDecl (typeof expr) expr

{-
    Code generation state
-}

data GeneratorState = GeneratorState {
    counter :: Integer
}

type GeneratorStateT = StateT GeneratorState IO

uniqueId :: GeneratorStateT Integer
uniqueId = do
    c <- gets counter
    put $ GeneratorState {
        counter = succ c
    }

    return c

type BlockGeneratorT = WriterT [BasicBlock] GeneratorStateT
type StatementGeneratorT = WriterT [Statement] BlockGeneratorT

{-
	Objective-C syntax structures
-}

-- Represents any value for which a C/Objective-C type can be obtained
class Typeof a where
    typeof :: a -> Type

-- Any C or Objective-C identifier
-- (Selectors are not considered identifiers)
newtype Identifier = Identifier String
    deriving Eq

instance Show Identifier where
	show (Identifier s) = s

isLegalIdentifier :: String -> Bool
isLegalIdentifier "" = False
isLegalIdentifier (x:xs) =
    let isAlphaUnderscore :: Char -> Bool
        isAlphaUnderscore c = (isAlpha c) || (c == '_')

        isAlphaNumUnderscore :: Char -> Bool
        isAlphaNumUnderscore c = (isAlphaNum c) || (c == '_')
    in isAlphaUnderscore x && (null $ dropWhile isAlphaNumUnderscore xs)

-- Objective-C selectors
newtype Selector = Selector String
    deriving Eq

instance Typeof Selector where
    typeof s = SelectorType

instance Show Selector where
    show (Selector s) = "@selector(" ++ s ++ ")"

selectorParts :: Selector -> [String]
selectorParts (Selector s) =
    let selectorParts' :: String -> [String] -> [String]
        selectorParts' s acc =
            let (part, rest) = break (== ':') s
            in case rest of
                [] -> acc ++ [part]
                ":" -> acc ++ [part ++ ":"]
                _ -> selectorParts' (tail rest) $ acc ++ [part ++ ":"]
    in selectorParts' s []

-- Basic language types
data Type =
    VoidType |
    IntType |
    DoubleType |
    CharType |
    BoolType |
    IdType |
    ClassType |
    -- An object instance; technically a pointer, but having a distinct type better represents usage
    InstanceType Identifier |
    SelectorType |
    PointerType Type |
    -- Used interchangably for both function pointers and function prototypes
    FunctionType Type [Type] |
    BlockType Type [Type] |
    MethodType Type [Type]
    deriving Eq

instance Typeof Type where
    typeof t = t

instance Show Type where
    show VoidType = "void"
    show IntType = "int"
    show DoubleType = "double"
    show CharType = "char"
    show BoolType = "BOOL"
    show IdType = "id"
    show ClassType = "Class"
    show (InstanceType c) = (show c) ++ " *"
    show SelectorType = "SEL"
    show (PointerType t) = (show t) ++ " *"
    show (FunctionType r params) = (show r) ++ " (*)(" ++ (showDelimList ", " params) ++ ")"
    show (BlockType r params) = (show r) ++ " (^)(" ++ (showDelimList ", " params) ++ ")"
    -- MethodTypes have no universal code representation

-- Function prototypes
data FuncProto = FuncProto { funcType :: Type, funcName :: Identifier, funcParams :: [Identifier] }
    deriving Eq

instance Typeof FuncProto where
    typeof p = funcType p

instance Show FuncProto where
    show (FuncProto (FunctionType r pts) id pns) =
        let showNamedParam :: (Identifier, Type) -> String
            showNamedParam (p, t) = (show t) ++ " " ++ (show p)
        in (show r) ++ " " ++ (show id) ++ " (" ++ (intercalate ", " $ map showNamedParam $ zip pns pts) ++ ")"

-- Method prototypes
data MethodProto = MethodProto { isClassMethod :: Bool, methodType :: Type, methodName :: Selector, methodParams :: [Identifier] }
    deriving Eq

instance Typeof MethodProto where
    typeof p = methodType p

instance Show MethodProto where
    show (MethodProto c (MethodType r pts) sel pns) =
        let showMethodParts :: [String] -> [Identifier] -> [Type] -> String
            showMethodParts [selpart] [] [] = selpart
            showMethodParts selparts pns pts =
                let showPart :: (String, Identifier, Type) -> String
                    showPart (s, p, t) = s ++ "(" ++ (show t) ++ ")" ++ (show p)
                in intercalate " " $ map showPart $ zip3 selparts pns pts
        in (if c then '+' else '-') : " (" ++ (show r) ++ ")" ++ (showMethodParts (selectorParts sel) pns pts)

-- Basic syntactical unit for file scope
data BasicBlock =
    LocalImport FilePath |
    SystemImport FilePath |
    FuncDecl FuncProto |
    FuncDef FuncProto [Statement] |
    ObjcInterface { className :: Identifier, superclass :: Identifier, protocols :: [Identifier], decls :: [ObjcDecl] } |
    ObjcProtocol { protocolName :: Identifier, protocols :: [Identifier], decls :: [ObjcDecl] } |
    -- Can also be used for class extensions (with an empty categoryName)
    ObjcCategory { className :: Identifier, categoryName :: Identifier, decls :: [ObjcDecl] } |
    ObjcImplementation { className :: Identifier, categoryName :: Identifier, defs :: [ObjcDef] }
    deriving Eq

instance Show BasicBlock where
    show (LocalImport path) = "#import \"" ++ path ++ "\""
    show (SystemImport path) = "#import <" ++ path ++ ">"
    show (FuncDecl p) = (show p) ++ ";"
    show (FuncDef p stmts) = (show p) ++ " {\n" ++ (showDelimList "\n" stmts) ++ "\n}"
    show (ObjcInterface c sc ps decls) =
        "@interface " ++ (show c) ++ " : " ++ (show sc) ++ " <" ++ (showDelimList ", " ps) ++ ">\n" ++ (showDelimList "\n" decls) ++ "\n@end"

    show (ObjcProtocol n ps decls) =
        -- Aggressively forward-declare dependent protocols
        (concatMap (\p -> "@protocol " ++ (show p) ++ ";\n") ps) ++
        "@protocol " ++ (show n) ++ " <" ++ (showDelimList ", " ps) ++ ">\n" ++ (showDelimList "\n" decls) ++ "\n@end"

    show (ObjcCategory cl cat decls) =
        "@interface " ++ (show cl) ++ " (" ++ (show cat) ++ ")\n" ++ (showDelimList "\n" decls) ++ "\n@end"

    show (ObjcImplementation cl (Identifier cat) defs) =
        "@implementation " ++ (show cl) ++ (if null cat then "\n" else " (" ++ cat ++ ")\n") ++ (showDelimList "\n" defs) ++ "\n@end"

-- BasicBlocks can be sorted by the order they should appear in generated code
instance Ord BasicBlock where
    -- Imports
    compare SystemImport {} _ = LT
    compare _ SystemImport {} = GT
    compare LocalImport {} _ = LT
    compare _ LocalImport {} = GT

    -- Declarations
    compare FuncDecl {} _ = LT
    compare _ FuncDecl {} = GT
    compare ObjcProtocol {} _ = LT
    compare _ ObjcProtocol {} = GT

    -- Superclasses need to be fully declared before their subclasses
    -- (Forward declarations will not work)
    compare ObjcInterface { className = lc, superclass = lsc } ObjcInterface { className = rc, superclass = rsc } =
        if lc == rsc
            then LT
            else if rc == lsc
                then GT
                else EQ

    compare ObjcInterface {} ObjcCategory {} = LT
    compare ObjcCategory {} ObjcInterface {} = GT

    -- Definitions
    compare FuncDef {} _ = GT
    compare _ FuncDef {} = LT
    compare ObjcImplementation {} _ = GT
    compare _ ObjcImplementation {} = LT

    compare _ _ = EQ

-- Attributes for @property declarations
data PropertyAttribute =
    Nonatomic |
    Readonly |
    Readwrite |
    Assign |
    Strong |
    Weak |
    UnsafeUnretained |
    Copy |
    Getter Selector
    deriving Eq

instance Show PropertyAttribute where
    show Nonatomic = "nonatomic"
    show Readonly = "readonly"
    show Readwrite = "readwrite"
    show Assign = "assign"
    show Strong = "strong"
    show Weak = "weak"
    show UnsafeUnretained = "unsafe_unretained"
    show Copy = "copy"
    show (Getter sel) = "getter = " ++ (show sel)

-- Declarations for Objective-C constructs
data ObjcDecl =
    PropertyDecl [PropertyAttribute] Type Identifier |
    MethodDecl MethodProto |
    ProtocolRequired |
    ProtocolOptional
    deriving Eq

instance Typeof ObjcDecl where
    typeof (PropertyDecl _ t _) = t
    typeof (MethodDecl p) = typeof p

instance Show ObjcDecl where
    show (PropertyDecl attrs t id) = "@property (" ++ (showDelimList ", " attrs) ++ ") " ++ (show t) ++ " " ++ (show id) ++ ";"
    show (MethodDecl proto) = (show proto) ++ ";"
    show ProtocolRequired = "@required"
    show ProtocolOptional = "@optional"

-- Definitions for Objective-C constructs
data ObjcDef =
    MethodDef MethodProto [Statement]
    deriving Eq

instance Show ObjcDef where
    show (MethodDef p stmts) = (show p) ++ " {\n" ++ (showDelimList "\n" stmts) ++ "\n}"

-- Any C or Objective-C expression
data Expr =
    VoidExpr |
    NilLiteral |
    NullLiteral |
    BoolLiteral Bool |
    IntLiteral Int |
    DoubleLiteral Double |
    SelectorLiteral Selector |
    NSStringLiteral String |
    NSArrayLiteral [Expr] |
    NSDictionaryLiteral [(Expr, Expr)] |
    IdentExpr Identifier |
    ToObjExpr Expr |
    AssignExpr Identifier Expr |
    MessageExpr Expr Selector [Expr] |
    VarargMessageExpr Expr Selector [Expr] [Expr] |
    CallExpr Expr [Expr]
    deriving Eq

instance Typeof Expr where
    typeof VoidExpr = VoidType
    typeof NilLiteral = IdType
    typeof NullLiteral = PointerType VoidType
    typeof BoolLiteral {} = BoolType
    typeof IntLiteral {} = IntType
    typeof DoubleLiteral {} = DoubleType
    typeof SelectorLiteral {} = SelectorType
    typeof NSStringLiteral {} = InstanceType $ Identifier "NSString"
    typeof NSArrayLiteral {} = InstanceType $ Identifier "NSArray"
    typeof NSDictionaryLiteral {} = InstanceType $ Identifier "NSDictionary"
    typeof (ToObjExpr expr) = IdType
    typeof (AssignExpr _ expr) = typeof expr
    -- Can't get the type of other expressions unless we decide to pass more type information around

instance Show Expr where
    show VoidExpr = "((void)0)"
    show NilLiteral = "nil"
    show NullLiteral = "NULL"
    show (BoolLiteral True) = "YES"
    show (BoolLiteral False) = "NO"
    show (IntLiteral n) = show n
    show (DoubleLiteral n) = show n
    show (SelectorLiteral s) = show s
    -- TODO: escape special characters
    show (NSStringLiteral s) = "@\"" ++ s ++ "\""
    show (NSArrayLiteral exprs) = "@[" ++ (showDelimList ", " exprs) ++ "]"
    show (NSDictionaryLiteral kvs) =
        let showPair :: (Expr, Expr) -> String
            showPair (k, v) = (show k) ++ ": " ++ (show v)
        in "@{" ++ (intercalate ", " $ map showPair kvs) ++ "}"

    show (IdentExpr id) = show id
    show (ToObjExpr expr) = "@(" ++ (show expr) ++ ")"
    show (AssignExpr id expr) = "(" ++ (show id) ++ " = " ++ (show expr) ++ ")"
    show (MessageExpr rec sel args) = show $ VarargMessageExpr rec sel args []
    show (VarargMessageExpr rec sel args varargs) =
        let showMessageParts :: [String] -> [Expr] -> String
            showMessageParts [] [] = ""
            showMessageParts [selpart] [] = 
                if null varargs
                    then selpart
                    else selpart ++ (showDelimList ", " varargs) ++ ", nil"

            showMessageParts (selpart:selparts) (arg:args) =
                selpart ++ (show arg) ++ " " ++ (showMessageParts selparts args)

        in "[" ++ (show rec) ++ " " ++ (showMessageParts (selectorParts sel) args) ++ "]"

    show (CallExpr expr args) = (show expr) ++ "(" ++ (showDelimList ", " args) ++ ")"

-- Statements within a function, method, or block body
data Statement =
    EmptyStatement |
    Statement Expr |
    Declaration Type Identifier Expr |
    Return Expr
    deriving Eq

instance Show Statement where
    show EmptyStatement = "\t;"
    show (Statement expr) = "\t" ++ (show expr) ++ ";"
    show (Declaration t id expr) = "\t" ++ (show t) ++ " " ++ (show id) ++
        case expr of
        VoidExpr -> ";"
        expr -> " = " ++ (show expr) ++ ";"

    show (Return VoidExpr) = "\treturn;"
    show (Return expr) = "\treturn " ++ (show expr) ++ ";"
