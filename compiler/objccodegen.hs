module ObjcCodeGen (codegen, codegenToFile)
    where

import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Char
import qualified Data.Foldable as Foldable
import Data.List
import Data.Ratio
import qualified Data.Sequence as Seq
import System.IO
import Util

type Seq = Seq.Seq

codegen :: [A.Form] -> String
codegen forms = showDelimited "\n" $ sort $ codegenMain forms

codegenToFile :: FilePath -> [A.Form] -> IO ()
codegenToFile path forms = do
    fd <- openFile path WriteMode

    hPutStrLn fd $ codegen forms
    hClose fd

codegenMain :: [A.Form] -> [BasicBlock]
codegenMain forms =
    let (stmts, blocks) = runWriter $ genForms forms

        imports = [SystemImport "Foundation/Foundation.h", SystemImport "objc/runtime.h", LocalImport "CocoaClojureRuntime.h"]
        mainType = FunctionType IntType [IntType, PointerType (PointerType CharType)]
        mainProto = FuncProto {
            funcType = mainType,
            funcName = (PlainIdentifier "main"),
            funcParams = map PlainIdentifier ["argc", "argv"]
        }
    
        retStmt = Return $ IntLiteral 0
        mainDef = FuncDef mainProto $ [AutoreleasePool (stmts ++ [retStmt])]

    in imports ++ (mainDef : (Foldable.toList blocks))

genForms :: [A.Form] -> Writer (Seq BasicBlock) [Statement]
genForms [] = return []
genForms (form : rest) = do
    expr <- genForm form
    stmts <- genForms rest

    return $ (Statement expr) : stmts

-- Emits code for a form
-- Returns an expression representing the value of the form
genForm :: A.Form -> Writer (Seq BasicBlock) Expr
genForm A.EmptyForm = return VoidExpr
genForm (A.StringLiteral s) = return $ NSStringLiteral s
genForm (A.BooleanLiteral b) = return $ ToObjExpr $ BoolLiteral b
genForm (A.CharacterLiteral c) = return $ NSStringLiteral [c]
genForm A.NilLiteral = return extNilExpr

-- TODO: handle numbers bigger than an int
genForm (A.IntegerLiteral n) = return $ ToObjExpr $ IntLiteral $ fromInteger n

genForm (A.DecimalLiteral n) = case maybeDouble n of
    (Just dbl) -> return $ ToObjExpr $ DoubleLiteral dbl
    Nothing ->
        -- TODO: this isn't really arbitrary precision -- we need something closer to Clojure ratios
        let num = nsDecimalNumberExpr $ show $ numerator n
            dec = nsDecimalNumberExpr $ show $ denominator n
        in return $ MessageExpr num (Selector "decimalNumberByDividingBy:") [dec]

genForm (A.VectorLiteral forms) = do
    exprs <- mapM genForm forms
    return $ NSArrayLiteral exprs

genForm (A.MapLiteral kvs) = do
    let (keys, values) = unzip kvs

    keyExprs <- mapM genForm keys
    valueExprs <- mapM genForm values

    return $ NSDictionaryLiteral $ zip keyExprs valueExprs

genForm (A.SetLiteral forms) = do
    exprs <- mapM genForm forms

    let rec = IdentExpr $ PlainIdentifier "NSSet"
        sel = Selector "setWithObjects:"

    return $ MessageExpr rec sel $ exprs ++ [NilLiteral]

genForm (A.Symbol s) = return $ IdentExpr $ escapedIdentifier s

genForm (A.List []) = return $ listExpr []
genForm (A.List ((A.Symbol sym):xs))
    | sym == "def" = do
        let ((A.Symbol var):forms) = xs
            initForm = if null forms then A.EmptyForm else head forms
            id = escapedIdentifier var

        expr <- genForm initForm

        -- TODO: avoid redeclarations
        -- TODO: handle VoidExpr in the declaration type
        tell $ Seq.singleton $ GlobalVarDecl (typeof expr) id VoidExpr

        let updateStmt = case expr of
                         VoidExpr -> EmptyStatement
                         _ -> Statement $ AssignExpr (IdentExpr id) expr

        -- Returns a compound expression that yields the address of the variable
        return $ CompoundExpr [updateStmt, Statement $ AddrOfExpr id]

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
        return $ CompoundExpr $ map Statement exprs

    | sym == "let" = do
        let ((A.VectorLiteral bindings):forms) = xs

        decls <- genBindings bindings
        exprs <- mapM genForm forms

        return $ CompoundExpr $ decls ++ (map Statement exprs)

    | sym == "quote" =
        -- We only need head since (quote ...) only quotes and returns the first form.
        genQuotedForm $ head xs 

    -- TODO
    | sym == "var" = return $ VoidExpr

    | sym == "fn" = do
        -- TODO: support name? param
        -- TODO: support overloaded invoke methods
        let ((A.VectorLiteral params):forms) = xs

        exprs <- mapM genForm forms

        let paramIds = map (\(A.Symbol p) -> escapedIdentifier p) params
            aliases = aliasDecls paramIds

            lastExpr = last exprs

        return $ BlockLiteral {
            retType = Just IdType,

            -- TODO: support rest params
            blockParams = map (\p -> (IdType, p)) paramIds,
            blockStmts = aliases ++ [Label $ PlainIdentifier "_recur"] ++ (map Statement $ init exprs) ++ [Return lastExpr]
        }

    | sym == "loop" = do
        let ((A.VectorLiteral bindings):forms) = xs

        decls <- genBindings bindings
        exprs <- mapM genForm forms

        let aliases = aliasDecls $ map (\(Declaration _ id _) -> id) decls
            block = BlockLiteral {
                retType = Just IdType,
                blockParams = [],
                blockStmts = decls ++ aliases ++ [Label $ PlainIdentifier "_recur"] ++ (map Statement $ init exprs) ++ [Return $ last exprs]
            }

        return $ CallExpr block []

    | sym == "recur" = do
        exprs <- mapM genForm xs

        let aliases = aliasIds $ length exprs
            
            updateAlias :: (Expr, Identifier) -> Statement
            updateAlias (expr, id) = Statement $ AssignExpr (DerefExpr $ IdentExpr $ id) expr

            updateStmts = map updateAlias $ zip exprs aliases

        return $ CompoundExpr $ updateStmts ++ [Goto $ PlainIdentifier "_recur", Statement NilLiteral]
        
    -- TODO
    | sym == "throw" = return $ VoidExpr

    -- TODO
    | sym == "try" = return $ VoidExpr

    {- TODO: these?
    | sym == "monitor-exit" = return $ VoidExpr
    | sym == "monitor-enter" = return $ VoidExpr
    -}

    | sym == "." = do
        let (objForm:selForm:argForms) = xs

        obj <- genForm objForm
        args <- mapM genForm argForms

        case selForm of
            -- TODO: this won't handle bindings that contain SEL values
            (A.Symbol s) -> return $ MessageExpr obj (Selector s) args
            _ -> do
               sel <- genForm selForm
               -- TODO: emit code to invoke a variable selector (using NSInvocation)
               return $ VoidExpr

    -- TODO
    | sym == "set!" = return $ VoidExpr

genForm (A.List forms) = do
    exprs <- mapM genForm forms

    -- TODO: map void return types to nil
    return $ CallExpr (head exprs) (tail exprs)

-- Generates a quoted expression from a form.
genQuotedForm :: A.Form -> Writer (Seq BasicBlock) Expr

-- TODO: this almost certainly isn't right
genQuotedForm (A.Symbol s) = return $ NSStringLiteral $ show $ escapedIdentifier s

genQuotedForm (A.List xs) = do
    exprs <- mapM genQuotedForm xs
    return $ listExpr $ exprs

genQuotedForm form = genForm form

-- Returns declarations which initialize local bindings
genBindings :: [A.Form] -> Writer (Seq BasicBlock) [Statement]
genBindings [] = return []
genBindings ((A.Symbol s):form:xs) = do
    let id = escapedIdentifier s

    expr <- genForm form
    rest <- genBindings xs
    return $ Declaration (typeof expr) id expr : rest

-- Generates initialized pointers to a set of identifiers
-- The aliases can later be referenced using aliasIds
aliasDecls :: [Identifier] -> [Statement]
aliasDecls decls =
    let aliasDecls' :: [Identifier] -> Int -> [Statement]
        aliasDecls' [] _ = []
        aliasDecls' (x:xs) n =
            let t = PointerType $ InferredType $ IdentExpr x
                id = PlainIdentifier $ "_a" ++ (show n)
            in Declaration t id (AddrOfExpr x) : aliasDecls' xs (n + 1)
    in aliasDecls' decls 0

-- Returns N identifiers for declarations previously generated by aliasDecls
aliasIds :: Int -> [Identifier]
aliasIds 0 = []
aliasIds n = (aliasIds $ n - 1) ++ [PlainIdentifier $ "_a" ++ (show $ n - 1)]

-- An expression for the EXTNil singleton
extNilExpr :: Expr
extNilExpr = MessageExpr (IdentExpr $ PlainIdentifier "EXTNil") (Selector "null") []

-- Create a list with the given expressions.
listExpr :: [Expr] -> Expr
listExpr xs = MessageExpr (IdentExpr $ PlainIdentifier "CLJList") (Selector "listWithValues:") $ xs ++ [NilLiteral]

-- Given a string representing a decimal number, returns an expression to construct an NSDecimalNumber from it
nsDecimalNumberExpr :: String -> Expr
nsDecimalNumberExpr str = MessageExpr (IdentExpr $ PlainIdentifier "NSDecimalNumber") (Selector "decimalNumberWithString:") [NSStringLiteral str]

-- Invokes -isEqual: against the first expression, with the second expression as the argument
isEqualExpr :: Expr -> Expr -> Expr
isEqualExpr a b = MessageExpr a (Selector "isEqual:") [b]

{-
    Objective-C syntax structures
-}

-- Represents any value for which a C/Objective-C type can be obtained
class Typeof a where
    typeof :: a -> Type

-- Any C or Objective-C identifier
-- (Selectors are not considered identifiers)
data Identifier = 
    PlainIdentifier String |
    RestIdentifier String
    deriving Eq

instance Show Identifier where
    show (PlainIdentifier s) = s
    show (RestIdentifier s) = s ++ ", ..."

-- Takes an arbitrary string and makes it into a valid Objective-C identifier
escapedIdentifier :: String -> Identifier
escapedIdentifier "&" = RestIdentifier "_restArg_"
escapedIdentifier (x:xs) =
    let escapedChar :: Char -> String
        escapedChar c = "S" ++ (show $ ord c)

        isAlphaUnderscore :: Char -> Bool
        isAlphaUnderscore c = (isAlpha c) || (c == '_')

        isAlphaNumUnderscore :: Char -> Bool
        isAlphaNumUnderscore c = (isAlphaNum c) || (c == '_')

        escapedX = if isAlphaUnderscore x then [x] else escapedChar x
        escapedXS = concatMap (\c -> if isAlphaNumUnderscore c then [c] else escapedChar c) xs
    in PlainIdentifier $ escapedX ++ escapedXS

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
    show (FunctionType r params) = (show r) ++ " (*)(" ++ (showDelimited ", " params) ++ ")"
    show (BlockType r params) = (show r) ++ " (^)(" ++ (showDelimited ", " params) ++ ")"
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
    GlobalVarDecl Type Identifier Expr |
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
    show (FuncDef p stmts) = (show p) ++ " {\n" ++ (showDelimited "\n" stmts) ++ "\n}"
    show (GlobalVarDecl t id expr) = showVarDeclaration t id expr
    show (ObjcInterface c sc ps decls) =
        "@interface " ++ (show c) ++ " : " ++ (show sc) ++ " <" ++ (showDelimited ", " ps) ++ ">\n" ++ (showDelimited "\n" decls) ++ "\n@end"

    show (ObjcProtocol n ps decls) =
        -- Aggressively forward-declare dependent protocols
        (concatMap (\p -> "@protocol " ++ (show p) ++ ";\n") ps) ++
        "@protocol " ++ (show n) ++ " <" ++ (showDelimited ", " ps) ++ ">\n" ++ (showDelimited "\n" decls) ++ "\n@end"

    show (ObjcCategory cl cat decls) =
        "@interface " ++ (show cl) ++ " (" ++ (show cat) ++ ")\n" ++ (showDelimited "\n" decls) ++ "\n@end"

    show (ObjcImplementation cl (PlainIdentifier cat) defs) =
        "@implementation " ++ (show cl) ++ (if null cat then "\n" else " (" ++ cat ++ ")\n") ++ (showDelimited "\n" defs) ++ "\n@end"

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
    compare GlobalVarDecl {} _ = LT
    compare _ GlobalVarDecl {} = GT
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
    show (PropertyDecl attrs t id) = "@property (" ++ (showDelimited ", " attrs) ++ ") " ++ (show t) ++ " " ++ (show id) ++ ";"
    show (MethodDecl proto) = (show proto) ++ ";"
    show ProtocolRequired = "@required"
    show ProtocolOptional = "@optional"

-- Definitions for Objective-C constructs
data ObjcDef =
    MethodDef MethodProto [Statement]
    deriving Eq

instance Show ObjcDef where
    show (MethodDef p stmts) = (show p) ++ " {\n" ++ (showDelimited "\n" stmts) ++ "\n}"

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
    -- The first expression here must be a valid lvalue (e.g., IdentExpr, DerefExpr)
    AssignExpr Expr Expr |
    -- If more arguments are provided than the selector has parts, the message is assumed to use varargs
    MessageExpr Expr Selector [Expr] |
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
    typeof NSStringLiteral {} = InstanceType $ PlainIdentifier "NSString"
    typeof NSArrayLiteral {} = InstanceType $ PlainIdentifier "NSArray"
    typeof NSDictionaryLiteral {} = InstanceType $ PlainIdentifier "NSDictionary"
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
    -- TODO: handle other return types?
    typeof MessageExpr {} = IdType
    typeof (IfExpr _ expr _) = typeof expr
    typeof AndExpr {} = BoolType
    typeof OrExpr {} = BoolType
    typeof NotExpr {} = BoolType
    typeof expr = InferredType expr

instance Show Expr where
    show VoidExpr = "((void)0)"
    show NilLiteral = "((id)nil)"
    show NullLiteral = "NULL"
    show (BoolLiteral True) = "YES"
    show (BoolLiteral False) = "NO"
    show (IntLiteral n) = show n
    show (DoubleLiteral n) = show n
    show (SelectorLiteral s) = show s
    -- TODO: escape special characters
    show (NSStringLiteral s) = "@\"" ++ s ++ "\""
    show (NSArrayLiteral exprs) = "@[" ++ (showDelimited ", " exprs) ++ "]"
    show (NSDictionaryLiteral kvs) =
        let showPair :: (Expr, Expr) -> String
            showPair (k, v) = (show k) ++ ": " ++ (show v)
        in "@{" ++ (intercalate ", " $ map showPair kvs) ++ "}"

    show (BlockLiteral r ps stmts) = 
        let showNamedParam :: (Type, Identifier) -> String
            showNamedParam (t, p) = (show t) ++ " " ++ (show p)
        in "(^ " ++ (maybe "" show r) ++ " (" ++ (intercalate ", " $ map showNamedParam ps) ++ ") {\n" ++
            (showDelimited "\n" stmts) ++
            "\n})"

    show (CompoundExpr stmts) = "({\n" ++ (showDelimited "\n" stmts) ++ "\n})"
    show (IdentExpr id) = show id
    show (AddrOfExpr id) = "(&" ++ (show id) ++ ")"
    show (DerefExpr expr) = "(*" ++ (show expr) ++ ")"
    show (ToObjExpr expr) = "@(" ++ (show expr) ++ ")"
    show (AssignExpr lexpr rexpr) = "(" ++ (show lexpr) ++ " = " ++ (show rexpr) ++ ")"
    show (MessageExpr rec sel args) =
        let showMessageParts :: [String] -> [Expr] -> String
            showMessageParts [] [] = ""
            showMessageParts [selpart] [] = selpart
            showMessageParts [selpart] [arg] = selpart ++ (show arg)
            showMessageParts [selpart] args = selpart ++ (showDelimited ", " args)
            showMessageParts (selpart:selparts) (arg:args) =
                selpart ++ (show arg) ++ " " ++ (showMessageParts selparts args)

        in "[" ++ (show rec) ++ " " ++ (showMessageParts (selectorParts sel) args) ++ "]"

    show (CallExpr expr args) = (show expr) ++ "(" ++ (showDelimited ", " args) ++ ")"
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

-- Shows a value, with all lines indented by one level
showEntabbed :: Show a => a -> String
showEntabbed val = intercalate "\n" $ map ((:) '\t') $ lines $ show val

-- Shows an indented, newline-separated list of values
showEntabbedLines :: Show a => [a] -> String
showEntabbedLines vals = intercalate "\n" $ map showEntabbed vals

-- Shows a variable declaration
showVarDeclaration :: Type -> Identifier -> Expr -> String
showVarDeclaration (BlockType r pts) id expr =
    (show r) ++ " (^" ++ (show id) ++ ")(" ++ (showDelimited ", " pts) ++ ")" ++ (showInitExpr expr) ++ ";"

showVarDeclaration (FunctionType r pts) id expr =
    (show r) ++ " (*" ++ (show id) ++ ")(" ++ (showDelimited ", " pts) ++ ")" ++ (showInitExpr expr) ++ ";"

showVarDeclaration t id expr = (show t) ++ " " ++ (show id) ++ (showInitExpr expr) ++ ";"

-- Returns a string for use following a declaration, to initialize it with the given expression
showInitExpr :: Expr -> String
showInitExpr VoidExpr = ""
showInitExpr expr =
    let exprLines = lines $ show expr
        firstLine = head exprLines
        otherLines = map (\s -> '\t' : s) $ tail exprLines
    in " = " ++ (intercalate "\n" $ firstLine : otherLines)

instance Show Statement where
    show EmptyStatement = "\t;"
    show (Statement expr) = (showEntabbed expr) ++ ";"
    show (Declaration t id expr) = intercalate "\n" $ map ((:) '\t') $ lines $ showVarDeclaration t id expr
    show (AutoreleasePool stmts) = "\t@autoreleasepool {\n" ++ (showEntabbedLines stmts) ++ "\n\t}"
    show (Return VoidExpr) = "\treturn;"
    show (Return expr) = "\treturn " ++ (show expr) ++ ";"
    show (Label id) = (show id) ++ ":"
    show (Goto id) = "\tgoto " ++ (show id) ++ ";"
