module AST (
        Form(..),
        foldMapForm
    ) where

import Data.List
import Data.Monoid
import Data.Ratio
import Control.Applicative ((<$>))
import Util

data Form =
    EmptyForm |
    Symbol String |
    StringLiteral String |
    IntegerLiteral Integer |
    DecimalLiteral Rational |
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
    show (DecimalLiteral n) = case maybeDouble n of
        (Just dbl) -> show dbl
        Nothing -> show (numerator n) ++ "/" ++ show (denominator n)

    show (CharacterLiteral c) = case c of
        '\n' -> "\\newline"
        '\t' -> "\\tab"
        _ -> '\'':c:"'"

    show NilLiteral = "nil"
    show (BooleanLiteral True) = "true"
    show (BooleanLiteral False) = "false"
    show (List x) = "(" ++ showDelimList " " x ++ ")"
    show (Vector x) = "[" ++ showDelimList " " x ++ "]"
    show (Set x) = "#{" ++ showDelimList " " x ++ "}"
    show (Map kvs) =
        let showPair :: (Form, Form) -> String
            showPair (k, v) = show k ++ " " ++ show v
        in "{" ++ intercalate ", " (showPair <$> kvs) ++ "}"

-- Folds and concats over all forms in an AST
foldMapForm :: Monoid m => (Form -> m) -> Form -> m
foldMapForm f form@(Vector forms) = f form <> foldMapFormList f forms
foldMapForm f form@(Set forms) = f form <> foldMapFormList f forms
foldMapForm f form@(Map kvps) =
    let kvf (k, v) = foldMapForm f k <> foldMapForm f v
    in f form <> mconcat (kvf <$> kvps)

-- Don't fold quoted lists (but do pass the list itself into the function)
foldMapForm f form@(List ((Symbol "quote"):_)) = f form
foldMapForm f form@(List forms) = f form <> foldMapFormList f forms

foldMapForm f form = f form

foldMapFormList :: Monoid m => (Form -> m) -> [Form] -> m
foldMapFormList f forms = mconcat $ foldMapForm f <$> forms
