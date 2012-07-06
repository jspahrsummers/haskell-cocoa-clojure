module LLVMCodeGen (putForms)
    where

import qualified AST as A
import System.IO
import Util

putForms :: Handle -> [A.Form] -> IO ()
putForms fd forms = putStrLn "not implemented"

newtype GlobalSymbol = GlobalSymbol String
instance Show GlobalSymbol where
    show (GlobalSymbol s) = "@" ++ s

newtype LocalSymbol = LocalSymbol String
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
instance Show Prototype where
    show (Prototype (FunctionType r args) sym) = (show r) ++ (show sym) ++ " (" ++ (showDelimList ", " args) ++ ")"

data BasicBlock =
    FuncDecl Prototype |
    FuncDef Prototype [Statement] |
    TypeDecl Type LocalSymbol |
    ConstantDecl GlobalSymbol Value

instance Show BasicBlock where
    show (FuncDecl p) = "declare " ++ (show p) ++ "\n"
    show (FuncDef p sl) = "define " ++ (show p) ++ " {\n" ++ (concatMap show sl) ++ "}\n\n"
    show (TypeDecl t sym) = (show sym) ++ " = type " ++ (show t)
    show (ConstantDecl sym v) = (show sym) ++ " = private unnamed_addr constant " ++ (show v) ++ "\n"

data Variable = Variable Type LocalSymbol
instance Show Variable where
    show (Variable _ sym) = show sym

data Value =
    IntegerLiteral Type Integer |
    FloatLiteral Type Double |
    StringLiteral String |
    LocalValue Variable

instance Show Value where
    show (IntegerLiteral t v) = (show t) ++ " " ++ (show v)
    show (FloatLiteral t v) = (show t) ++ " " ++ (show v)
    show (LocalValue (Variable t sym)) = (show t) ++ " " ++ (show sym)

    -- StringLiterals are only ever used for constants
    -- TODO: escape special characters
    show (StringLiteral str) = (show $ ArrayType (length str) (IntegerType 8)) ++ " c\"" ++ (show str) ++ "\""

data Expression =
    Call Prototype [Value] |
    GetElementPtr Type GlobalSymbol [Value] |
    Ret Value

instance Show Expression where
    show (Call (Prototype t sym) args) = "call " ++ (show $ PointerType t) ++ " " ++ (show sym) ++ " (" ++ (showDelimList ", " args) ++ ")"
    show (GetElementPtr t sym indexes) = "getelementptr " ++ (show $ PointerType t) ++ " " ++ (show sym) ++ " ," ++ (showDelimList ", " indexes)
    show (Ret v) = "ret " ++ (show v)

data Statement =
    Label String |
    Statement Expression |
    AssignStatement Variable Expression |
    UncondBr LocalSymbol |
    Unreachable

instance Show Statement where
    show (Label l) = l ++ ":\n"
    show (Statement expr) = "\t" ++ (show expr) ++ "\n"
    show (AssignStatement var expr) = "\t" ++ (show var) ++ " = " ++ (show expr) ++ "\n"
    show (UncondBr sym) = "\tbr label " ++ (show sym) ++ "\n"
    show Unreachable = "\tunreachable\n"
