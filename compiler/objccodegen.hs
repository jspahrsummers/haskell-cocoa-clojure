module ObjcCodeGen (codegen, codegenToFile)
    where

import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Char
import Data.List
import System.IO
import Util

codegen :: [A.Form] -> String
codegen forms =
    let st = GeneratorState { counter = 0 }
    in showDelimList "\n" $ sort $ evalState (codegenMain forms) st

codegenToFile :: Handle -> [A.Form] -> IO ()
codegenToFile fd forms = hPutStrLn fd $ codegen forms

codegenMain :: [A.Form] -> GeneratorStateT [BasicBlock]
codegenMain forms = do
    (stmts, blocks) <- runWriterT $ genForms forms

    let imports = [SystemImport "Foundation/Foundation.h", SystemImport "objc/runtime.h", LocalImport "CocoaClojureRuntime.h"]
        mainType = FunctionType IntType [IntType, PointerType (PointerType CharType)]
        mainProto = FuncProto {
            funcType = mainType,
            funcName = (Identifier "main"),
            funcParams = map Identifier ["argc", "argv"]
        }
    
        retStmt = Return $ IntLiteral 0
        mainDef = FuncDef mainProto $ [AutoreleasePool (stmts ++ [retStmt])]

    return $ imports ++ (mainDef : blocks)

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
genForm (A.StringLiteral s) = return $ NSStringLiteral s
genForm (A.BooleanLiteral b) = return $ ToObjExpr $ BoolLiteral b
genForm (A.CharacterLiteral c) = return $ NSStringLiteral [c]
genForm A.NilLiteral = return extNilExpr

-- TODO: handle numbers bigger than an int
genForm (A.IntegerLiteral n) = return $ ToObjExpr $ IntLiteral $ fromInteger n

-- TODO: handle BigDecimals
genForm (A.DecimalLiteral n) = return $ ToObjExpr $ DoubleLiteral n

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

genForm (A.Symbol s) = return $ IdentExpr $ escapedIdentifier s

-- TODO: generate code for an empty list
--genForm (A.List [])

-- TODO: special forms
genForm (A.List ((A.Symbol sym):xs))
    | sym == "def" = return $ VoidExpr
    | sym == "if" = do
        exprs <- mapM genForm xs

        let (test:thenExpr:xs') = exprs
            elseExpr = if null xs'
                then extNilExpr
                else head xs'

            -- !([[EXTNil null] isEqual:test] || [@NO isEqual:test])
            cond = NotExpr $ OrExpr (isEqualExpr extNilExpr test) $ isEqualExpr (ToObjExpr $ BoolLiteral False) test

        return $ IfExpr cond thenExpr elseExpr

    | sym == "do" = do
        exprs <- mapM genForm xs
        tell $ map Statement $ init exprs

        return $ last exprs

    | sym == "let" = do
        let ((A.Vector bindings):forms) = xs

        decls <- genBindings bindings
        exprs <- mapM genForm forms

        return $ CompoundExpr $ decls ++ (map Statement exprs)

    | sym == "quote" = return $ VoidExpr
    | sym == "var" = return $ VoidExpr
    | sym == "fn" = do
        -- TODO: support name? param
        -- TODO: support overloaded invoke methods
        let ((A.Vector params):forms) = xs

        exprs <- mapM genForm forms

        let lastExpr = last exprs
            lastType = typeof lastExpr

            rettype = case lastType of
                      VoidType -> Nothing
                      t -> Just t

            retstmt = case lastType of
                      VoidType -> Statement lastExpr
                      t -> Return lastExpr

        genUniqueInitDecl $ BlockLiteral {
            retType = rettype,

            -- TODO: support rest params
            blockParams = map (\(A.Symbol p) -> (IdType, escapedIdentifier p)) params,
            blockStmts = (map Statement (init exprs)) ++ [retstmt]
        }

    | sym == "loop" = return $ VoidExpr
    | sym == "recur" = return $ VoidExpr
    | sym == "throw" = return $ VoidExpr
    | sym == "try" = return $ VoidExpr
    {- TODO: these?
    | sym == "monitor-exit" = return $ VoidExpr
    | sym == "monitor-enter" = return $ VoidExpr
    -}
    | sym == "." = return $ VoidExpr
    | sym == "set!" = return $ VoidExpr

genForm (A.List forms) = do
    exprs <- mapM genForm forms

    -- TODO: map void return types to nil
    return $ CallExpr (head exprs) (tail exprs)

-- TODO
--genForm (A.RationalLiteral n) =

-- Returns declarations which initialize local bindings
genBindings :: [A.Form] -> StatementGeneratorT [Statement]
genBindings [] = return []
genBindings ((A.Symbol s):form:xs) = do
    let id = escapedIdentifier s

    expr <- genForm form
    rest <- genBindings xs
    return $ Declaration (typeof expr) id expr : rest

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

-- An expression for the EXTNil singleton
extNilExpr :: Expr
extNilExpr = MessageExpr (IdentExpr $ Identifier "EXTNil") (Selector "null") []

-- Invokes -isEqual: against the first expression, with the second expression as the argument
isEqualExpr :: Expr -> Expr -> Expr
isEqualExpr a b = MessageExpr a (Selector "isEqual:") [b]

{-
    Code generation state
-}

data GeneratorState = GeneratorState {
    counter :: Integer
}

type GeneratorStateT = State GeneratorState

uniqueId :: GeneratorStateT Integer
uniqueId = do
    c <- gets counter
    put $ GeneratorState {
        counter = succ c
    }

    return c

-- TODO: these writers should probably use a structure more efficient than a list
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

-- Takes an arbitrary string and makes it into a valid Objective-C identifier
escapedIdentifier :: String -> Identifier
escapedIdentifier (x:xs) =
    let escapedChar :: Char -> String
        escapedChar c = "S" ++ (show $ ord c)

        isAlphaUnderscore :: Char -> Bool
        isAlphaUnderscore c = (isAlpha c) || (c == '_')

        isAlphaNumUnderscore :: Char -> Bool
        isAlphaNumUnderscore c = (isAlphaNum c) || (c == '_')

        escapedX = if isAlphaUnderscore x then [x] else escapedChar x
        escapedXS = concatMap (\c -> if isAlphaNumUnderscore c then [c] else escapedChar c) xs
    in Identifier $ escapedX ++ escapedXS

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
    MethodType Type [Type] |
    InferredType Expr
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
    show (InferredType expr) = "__typeof__(" ++ (show expr) ++ ")"

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
    -- retType is required only if it cannot be inferred from Return statements
    BlockLiteral { retType :: Maybe Type, blockParams :: [(Type, Identifier)], blockStmts :: [Statement] } |
    CompoundExpr [Statement] |
    IdentExpr Identifier |
    AddrOfExpr Identifier |
    DerefExpr Expr |
    ToObjExpr Expr |
    AssignExpr Identifier Expr |
    MessageExpr Expr Selector [Expr] |
    VarargMessageExpr Expr Selector [Expr] [Expr] |
    CallExpr Expr [Expr] |
    -- Ternary operator
    IfExpr Expr Expr Expr |
    AndExpr Expr Expr |
    OrExpr Expr Expr |
    NotExpr Expr
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
    typeof (BlockLiteral (Just t) ps _) =
        let pts = fst $ unzip ps
        in BlockType t pts

    typeof bl@(BlockLiteral Nothing ps stmts) =
        let pts = fst $ unzip ps
        in BlockType (typeof $ last stmts) pts

    typeof cl@(CompoundExpr stmts) = typeof $ last stmts
    typeof AddrOfExpr {} = PointerType VoidType
    typeof dre@(DerefExpr expr) = case typeof expr of
        (PointerType t) -> t
        _ -> InferredType dre

    typeof ToObjExpr {} = IdType
    typeof (AssignExpr _ expr) = typeof expr
    typeof (IfExpr _ expr _) = typeof expr
    typeof AndExpr {} = BoolType
    typeof OrExpr {} = BoolType
    typeof NotExpr {} = BoolType
    typeof expr = InferredType expr

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

    show (BlockLiteral r ps stmts) = 
        let showNamedParam :: (Type, Identifier) -> String
            showNamedParam (t, p) = (show t) ++ " " ++ (show p)
        in "(^ " ++ (maybe "" show r) ++ " (" ++ (intercalate ", " $ map showNamedParam ps) ++ ") {\n" ++
            (showEntabbed stmts) ++
            "\n})"

    show (CompoundExpr stmts) = "({\n" ++ (showEntabbed stmts) ++ "\n})"
    show (IdentExpr id) = show id
    show (AddrOfExpr id) = "(&" ++ (show id) ++ ")"
    show (DerefExpr expr) = "(*" ++ (show expr) ++ ")"
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
    show (IfExpr cond trueExpr falseExpr) = "(" ++ (show cond) ++ " ? " ++ (show trueExpr) ++ " : " ++ (show falseExpr) ++ ")"
    show (AndExpr a b) = "(" ++ (show a) ++ " && " ++ (show b) ++ ")"
    show (OrExpr a b) = "(" ++ (show a) ++ " || " ++ (show b) ++ ")"
    show (NotExpr expr) = "(!" ++ (show expr) ++ ")"

-- Statements within a function, method, or block body
data Statement =
    EmptyStatement |
    Statement Expr |
    Declaration Type Identifier Expr |
    AutoreleasePool [Statement] |
    Return Expr |
    Label Identifier |
    Goto Identifier
    deriving Eq

instance Typeof Statement where
    typeof (Statement expr) = typeof expr
    typeof _ = VoidType

-- Entabs a list of statements
showEntabbed :: [Statement] -> String
showEntabbed stmts =
    let entabShow :: Statement -> String
        entabShow stmt = "\t" ++ (show stmt)
    in intercalate "\n" $ map entabShow stmts

showInitExpr :: Expr -> String
showInitExpr VoidExpr = ""
showInitExpr expr = " = " ++ (show expr)

instance Show Statement where
    show EmptyStatement = "\t;"
    show (Statement expr) = "\t" ++ (show expr) ++ ";"
    show (Declaration (BlockType r pts) id expr) =
        "\t" ++ (show r) ++ " (^" ++ (show id) ++ ")(" ++ (showDelimList ", " pts) ++ ")" ++ (showInitExpr expr) ++ ";"

    show (Declaration (FunctionType r pts) id expr) =
        "\t" ++ (show r) ++ " (*" ++ (show id) ++ ")(" ++ (showDelimList ", " pts) ++ ")" ++ (showInitExpr expr) ++ ";"

    show (Declaration t id expr) = "\t" ++ (show t) ++ " " ++ (show id) ++ (showInitExpr expr) ++ ";"
    show (AutoreleasePool stmts) = "\t@autoreleasepool {\n" ++ (showEntabbed stmts) ++ "\n\t}"
    show (Return VoidExpr) = "\treturn;"
    show (Return expr) = "\treturn " ++ (show expr) ++ ";"
    show (Label id) = (show id) ++ ":"
    show (Goto id) = "\tgoto " ++ (show id) ++ ";"
