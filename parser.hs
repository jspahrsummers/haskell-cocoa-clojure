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
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
symbol = P.symbol lexer

whiteSpace = skipMany (space <|> char ',')

stringLiteral = liftM StringLiteral $ P.stringLiteral lexer
identifier = liftM (SymbolForm . Symbol) $ P.identifier lexer

-- TODO: Clojure number parsing rules
-- TODO: ratio support
-- TODO: BigDecimal support
numberLiteral = liftM (either IntegerLiteral DecimalLiteral) $ P.naturalOrFloat lexer

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

mapLiteral =
    let keyValuePair = liftM (\[x,y] -> (x,y)) $ count 2 form
    in liftM Map $ braces (many keyValuePair)

setLiteral = do
    char '#'
    liftM Set $ braces (many form)

form = do
    whiteSpace
    nil <|> true <|> false <|>
        numberLiteral <|> stringLiteral <|> characterLiteral <|>
        setLiteral <|> listLiteral <|> vectorLiteral <|> mapLiteral <|>
        identifier

-- TODO: Move everything below here into a read table implementation
-- (i.e., they should be reader macros, not hardcoded)

characterLiteral = do
    let newlineLiteral = do
            symbol "newline"
            return '\n'
        spaceLiteral = do
            symbol "space"
            return ' '
        tabLiteral = do
            symbol "tab"
            return '\t'

    char '\\'
    liftM CharacterLiteral $
        try newlineLiteral <|>
        try spaceLiteral <|>
        try tabLiteral <|>
        anyChar
