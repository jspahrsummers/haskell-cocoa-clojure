{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser where

import AST
import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

symbolSpecialChar = oneOf "*+!-_?/."

-- Language definition parameters specific to Clojure
lexer = P.makeTokenParser emptyDef {
    P.reservedNames = ["true", "false", "nil"],
    P.identStart = letter <|> symbolSpecialChar,
    P.identLetter = alphaNum <|> symbolSpecialChar,
    P.commentLine = ";"
}

reserved = P.reserved lexer
stringLiteral = P.stringLiteral lexer
characterLiteral = P.charLiteral lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer

-- The naming here might be confusing for Parsec users, since P.symbol normally
-- matches any given string, but this matches Clojure terminology
symbol = P.identifier lexer

whiteSpace = skipMany (space <|> char ',')

-- TODO: Clojure number parsing rules
-- TODO: ratio support
-- TODO: BigDecimal support
number = liftM (either IntegerLiteral DecimalLiteral) $ P.naturalOrFloat lexer

nil = do
    reserved "nil"
    return NilLiteral

true = do
    reserved "true"
    return $ BooleanLiteral True

false = do
    reserved "false"
    return $ BooleanLiteral False

listLiteral = liftM List $ parens (many form)
vectorLiteral = liftM Vector $ brackets (many form)

tuple2 :: [a] -> (a,a)
tuple2 [x,y] = (x,y)

keyValuePair = liftM tuple2 $ count 2 form
mapLiteral = liftM Map $ braces (many keyValuePair)

setLiteral = do
    char '#'
    liftM Set $ braces (many form)

form = return NilLiteral
