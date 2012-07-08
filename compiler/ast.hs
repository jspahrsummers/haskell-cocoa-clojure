module AST (
        Form(..),
        collectSymbols,
        mapSymbols
    ) where

import Data.List
import Data.Ratio
import qualified Data.Set as Set
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
    show (MapLiteral kvs) =
        let showPair :: (Form, Form) -> String
            showPair (k, v) = show k ++ " " ++ show v
        in "{" ++ intercalate ", " (showPair <$> kvs) ++ "}"

-- Maps over a list and unions the resulting sets
unionMap :: Ord b => (a -> Set.Set b) -> [a] -> Set.Set b
unionMap f l = Set.unions $ map f l

-- Collects all symbols used within a form
collectSymbols :: Form -> Set.Set String
collectSymbols (Symbol s) = Set.singleton s
collectSymbols (List forms) = unionMap collectSymbols forms
collectSymbols (VectorLiteral forms) = unionMap collectSymbols forms
collectSymbols (SetLiteral forms) = unionMap collectSymbols forms
collectSymbols (MapLiteral kvps) =
    let (keys, values) = unzip kvps
    in (unionMap collectSymbols keys) `Set.union` (unionMap collectSymbols values)

collectSymbols _ = Set.empty

-- Maps over all Symbols within a form, allowing them to be renamed or replaced
mapSymbols :: (Form -> Form) -> Form -> Form
mapSymbols f form@(Symbol _) = f form
mapSymbols f (List forms) = List $ map (mapSymbols f) forms
mapSymbols f (VectorLiteral forms) = VectorLiteral $ map (mapSymbols f) forms
mapSymbols f (SetLiteral forms) = SetLiteral $ map (mapSymbols f) forms
mapSymbols f (MapLiteral kvps) =
    let (keys, values) = unzip kvps
    in MapLiteral $ zip (map (mapSymbols f) keys) (map (mapSymbols f) values)

mapSymbols f form = form
