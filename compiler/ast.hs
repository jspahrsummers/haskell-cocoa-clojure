module AST (
        Form(..),
        collectSymbols,
        innerForms,
        mapForm
    ) where

import Data.List
import Data.Ratio
import qualified Data.Set as Set
import Control.Applicative ((<$>))
import Util

type Set = Set.Set

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
    VectorLiteral [Form] |
    MapLiteral [(Form, Form)] |
    SetLiteral [Form]
    deriving (Eq, Ord)

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
    show (List x) = "(" ++ showDelimited " " x ++ ")"
    show (VectorLiteral x) = "[" ++ showDelimited " " x ++ "]"
    show (SetLiteral x) = "#{" ++ showDelimited " " x ++ "}"
    show (MapLiteral kvps) = 
        let showPair :: (Form, Form) -> String
            showPair (k, v) = show k ++ " " ++ show v
        in "{" ++ delimit ", " (map showPair kvps) ++ "}"

-- Returns any forms nested one level within the given form
innerForms :: Form -> [Form]
innerForms (List forms) = forms
innerForms (VectorLiteral forms) = forms
innerForms (SetLiteral forms) = forms
innerForms (MapLiteral kvps) =
    let (keys, values) = unzip kvps
    in keys ++ values

innerForms _ = []

-- Applies the function to the given form
-- For collections, the function is first applied to the elements, then the collection itself
mapForm :: (Form -> Form) -> Form -> Form
mapForm f (List forms) = f $ List $ map (mapForm f) forms
mapForm f (VectorLiteral forms) = f $ VectorLiteral $ map (mapForm f) forms
mapForm f (SetLiteral forms) = f $ SetLiteral $ map (mapForm f) forms
mapForm f (MapLiteral kvps) =
    let mapPair (k, v) = (mapForm f k, mapForm f v)
    in f $ MapLiteral $ map mapPair kvps

mapForm f form = f form

-- Collects all symbols used within a form
collectSymbols :: Form -> Set String
collectSymbols (Symbol s) = Set.singleton s
collectSymbols form = Set.unions $ map collectSymbols $ innerForms form
