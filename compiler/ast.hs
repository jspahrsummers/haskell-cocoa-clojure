module AST (
        Form(..),
        foldMapForm
    ) where

import Data.List
import Data.Monoid
import Data.Ratio
import Util

data Form =
    EmptyForm |
    Symbol String |
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
    Map [(Form, Form)] |
    Set [Form]

instance Show Form where
    show EmptyForm = ""
    show (Symbol s) = s
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
    show (List x) = "(" ++ (showDelimList " " x) ++ ")"
    show (Vector x) = "[" ++ (showDelimList " " x) ++ "]"
    show (Set x) = "#{" ++ (showDelimList " " x) ++ "}"
    show (Map kvs) =
        let showPair :: (Form, Form) -> String
            showPair (k, v) = (show k) ++ " " ++ (show v)
        in "{" ++ (intercalate ", " $ map showPair kvs) ++ "}"

-- Folds and concats over all forms in an AST
foldMapForm :: Monoid m => (Form -> m) -> Form -> m
foldMapForm f form@(List forms) = f form `mappend` foldMapFormList f forms
foldMapForm f form@(Vector forms) = f form `mappend` foldMapFormList f forms
foldMapForm f form@(Set forms) = f form `mappend` foldMapFormList f forms
foldMapForm f form@(Map kvps) =
    let kvf (k, v) = foldMapForm f k `mappend` foldMapForm f v
    in f form `mappend` mconcat (fmap kvf kvps)

foldMapForm f form = f form

foldMapFormList :: Monoid m => (Form -> m) -> [Form] -> m
foldMapFormList f forms = mconcat (fmap (foldMapForm f) forms)
