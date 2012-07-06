module AST (
        Form(..),
        KeyValuePair(..),
        Symbol(..),
        foldForm
    ) where

import Data.Monoid
import Data.Ratio
import Util

newtype Symbol = Symbol String
instance Show Symbol where
    show (Symbol s) = s

data KeyValuePair = KeyValuePair Form Form
instance Show KeyValuePair where
    show (KeyValuePair k v) = (show k) ++ " " ++ (show v)

data Form =
    EmptyForm |
    SymbolForm Symbol |
    StringLiteral String |
    IntegerLiteral Integer |
    RationalLiteral Rational |

    -- TODO: BigDecimals are currently not supported
    DecimalLiteral Double |

    CharacterLiteral Char |
    NilLiteral |
    BooleanLiteral Bool |
    List [Form] |
    Vector [Form] |
    Map [KeyValuePair] |
    Set [Form]

instance Show Form where
    show EmptyForm = ""
    show (SymbolForm s) = show s
    show (StringLiteral s) = "\"" ++ s ++ "\""
    show (IntegerLiteral n) = show n
    show (RationalLiteral n) = (show $ numerator n) ++ "/" ++ (show $ denominator n)
    show (DecimalLiteral n) = show n
    show (CharacterLiteral c) = case c of
        '\n' -> "\\newline"
        '\t' -> "\\tab"
        _ -> "'" ++ [c] ++ "'"

    show NilLiteral = "nil"
    show (BooleanLiteral True) = "true"
    show (BooleanLiteral False) = "false"
    show (List []) = "()"
    show (List x) = "(" ++ (showDelimList " " x) ++ ")"
    show (Vector []) = "[]"
    show (Vector x) = "[" ++ (showDelimList " " x) ++ "]"
    show (Set []) = "#{}"
    show (Set x) = "#{" ++ (showDelimList " " x) ++ "}"
    show (Map []) = "{}"
    show (Map x) = "{" ++ (showDelimList ", " x) ++ "}"

-- Folds over all forms in an AST
foldForm :: Monoid m => (Form -> m) -> Form -> m
foldForm f form@(List forms) = f form `mappend` foldFormList f forms
foldForm f form@(Vector forms) = f form `mappend` foldFormList f forms
foldForm f form@(Set forms) = f form `mappend` foldFormList f forms
foldForm f form@(Map kvps) =
    let kvf (KeyValuePair k v) = foldForm f k `mappend` foldForm f v
    in f form `mappend` mconcat (fmap kvf kvps)

foldForm f form = f form

foldFormList :: Monoid m => (Form -> m) -> [Form] -> m
foldFormList f forms = mconcat (fmap (foldForm f) forms)
