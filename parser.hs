{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser where

import AST
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

parens = P.parens lexer

-- The naming here might be confusing for Parsec users, since P.symbol matches
-- any given string, but this matches Clojure terminology
symbol = P.identifier lexer

whiteSpace = skipMany (space <|> char ',')

-- TODO: Clojure number parsing rules
-- TODO: ratio support
-- TODO: BigDecimal support
number = P.naturalOrFloat lexer

form = return NilLiteral
