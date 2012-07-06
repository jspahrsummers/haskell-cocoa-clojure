module LLVMCodeGen (codegen, codegenToFile)
    where

import qualified AST as A
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
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

    let mainType = FunctionType (IntegerType 32) [VoidType]
        mainProto = Prototype mainType (GlobalSymbol "main")
        retStmt = Ret $ IntegerLiteral (IntegerType 32) 0

    return $ FuncDef mainProto (stmts ++ [retStmt]) : blocks

genForms :: [A.Form] -> BlockGeneratorT [Statement]
genForms [] = return []
genForms (form : rest) = do
    (s1, _) <- genForm form
    s2 <- genForms rest
    return $ s1 ++ s2

genForm :: A.Form -> BlockGeneratorT ([Statement], Variable)
genForm A.EmptyForm = return ([], Nil)

genForm (A.StringLiteral s) = do
    id <- lift uniqueId

    let global = GlobalSymbol $ "str" ++ (show id)
        val = StringLiteral s
        var = Variable (typeof val) $ LocalSymbol ("str" ++ (show id))

    tell $ [ConstantDecl global val]

    let gep = GetElementPtr (typeof val) global [gepIndex 0, gepIndex 0]
        assign = AssignStatement var gep

    return $ ([assign], var)

{-
genForm (A.SymbolForm s) = 
genForm (A.StringLiteral s) = 
genForm (A.IntegerLiteral n) = 
genForm (A.RationalLiteral n) = 
genForm (A.DecimalLiteral n) = 
genForm (A.CharacterLiteral c) = 
genForm A.NilLiteral = 
genForm (A.BooleanLiteral b) = 
genForm (A.List x) = 
genForm (A.Vector x) = 
genForm (A.Set x) = 
genForm (A.Map x) = 
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

{-
    Definitions for LLVM code structures
-}
newtype GlobalSymbol = GlobalSymbol String
    deriving Eq

instance Show GlobalSymbol where
    show (GlobalSymbol s) = "@" ++ s

newtype LocalSymbol = LocalSymbol String
    deriving Eq

instance Show LocalSymbol where
    show (LocalSymbol s) = "%" ++ s

data Type =
    VoidType |
    OpaqueType |
    IntegerType Int |
    FloatType |
    DoubleType |
    PointerType Type |
    ArrayType Int Type |
    FunctionType Type [Type] |
    TypeAlias LocalSymbol
    deriving Eq

instance Show Type where
    show VoidType = "void"
    show OpaqueType = "opaque"
    show (IntegerType b) = "i" ++ (show b)
    show FloatType = "float"
    show DoubleType = "double"
    show (PointerType t) = (show t) ++ "*"
    show (ArrayType c t) = "[" ++ (show c) ++ " x " ++ (show t) ++ "]"
    show (FunctionType r args) = (show r) ++ " (" ++ (showDelimList ", " args) ++ ")"
    show (TypeAlias sym) = show sym

data Prototype = Prototype Type GlobalSymbol
    deriving Eq

instance Show Prototype where
    show (Prototype (FunctionType r args) sym) = (show r) ++ " " ++ (show sym) ++ " (" ++ (showDelimList ", " args) ++ ")"

data BasicBlock =
    FuncDecl Prototype |
    FuncDef Prototype [Statement] |
    TypeDecl Type LocalSymbol |
    ConstantDecl GlobalSymbol Expression
    deriving Eq

instance Show BasicBlock where
    show (FuncDecl p) = "declare " ++ (show p)
    show (FuncDef p sl) = "define " ++ (show p) ++ " {\n" ++ (showDelimList "\n" sl) ++ "\n}\n"
    show (TypeDecl t sym) = (show sym) ++ " = type " ++ (show t)
    show (ConstantDecl sym v) = (show sym) ++ " = private unnamed_addr constant " ++ (show v)

-- BasicBlocks can be sorted by the order they should appear in generated LLVM IR
instance Ord BasicBlock where
    compare (TypeDecl _ _) _ = LT
    compare _ (TypeDecl _ _) = GT
    compare _ (FuncDef _ _) = LT
    compare (FuncDef _ _) _ = GT
    compare (FuncDecl _) (ConstantDecl _ _) = GT
    compare (ConstantDecl _ _) (FuncDecl _) = LT
    compare _ _ = EQ

data Variable =
    Nil |
    Variable Type LocalSymbol
    deriving Eq

instance Show Variable where
    show (Variable _ sym) = show sym

data Expression =
    IntegerLiteral Type Integer |
    FloatLiteral Type Double |
    StringLiteral String |
    LocalValue Variable |
    Call Prototype [Expression] |
    GetElementPtr Type GlobalSymbol [Expression]
    deriving Eq

instance Show Expression where
    show (Call (Prototype t sym) args) = "call " ++ (show $ PointerType t) ++ " " ++ (show sym) ++ " (" ++ (showDelimList ", " args) ++ ")"
    show (GetElementPtr t sym indexes) = "getelementptr " ++ (show $ PointerType t) ++ " " ++ (show sym) ++ ", " ++ (showDelimList ", " indexes)
    show (IntegerLiteral t v) = (show t) ++ " " ++ (show v)
    show (FloatLiteral t v) = (show t) ++ " " ++ (show v)
    show (LocalValue (Variable t sym)) = (show t) ++ " " ++ (show sym)

    -- StringLiterals are only ever used for constants
    -- TODO: escape special characters
    show lit@(StringLiteral str) = (show $ typeof lit) ++ " c\"" ++ str ++ "\""

typeof :: Expression -> Type
typeof (IntegerLiteral t _) = t
typeof (FloatLiteral t _) = t
typeof (StringLiteral str) = ArrayType (length str) (IntegerType 8)
typeof (LocalValue (Variable t _)) = t
typeof (Call (Prototype (FunctionType t _) _) _) = t

-- TODO: this isn't actually correct for all cases -- simply the ones we'll have for the time being
typeof (GetElementPtr (PointerType t) _ _) = t

gepIndex :: Integer -> Expression
gepIndex i = (IntegerLiteral (IntegerType 32) i)

data Statement =
    Label String |
    Statement Expression |
    AssignStatement Variable Expression |
    UncondBr LocalSymbol |
    Unreachable |
    Ret Expression
    deriving Eq

instance Show Statement where
    show (Label l) = l ++ ":"
    show (Statement expr) = "\t" ++ (show expr)
    show (AssignStatement var expr) = "\t" ++ (show var) ++ " = " ++ (show expr)
    show (UncondBr sym) = "\tbr label " ++ (show sym)
    show Unreachable = "\tunreachable"
    show (Ret v) = "\tret " ++ (show v)
