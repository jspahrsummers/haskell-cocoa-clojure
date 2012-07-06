{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser (forms)
    where

import AST
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

symbolSpecialChar = oneOf "*+!-_?/.%:"

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
    let keyValuePair = liftM (\[x,y] -> (KeyValuePair x y)) $ count 2 form
    in liftM Map $ braces (many keyValuePair)

setLiteral = do
    try $ char '#'
    liftM Set $ braces (many form)

form = do
    whiteSpace

    nil <|> true <|> false <|>
        -- reader macros
        ignoreNext <|> quotedForm <|> deref <|> varQuote <|> anonymousFunction <|>

        numberLiteral <|> stringLiteral <|> characterLiteral <|>
        setLiteral <|> listLiteral <|> vectorLiteral <|> mapLiteral <|>

        identifier

forms = many form

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

ignoreNext = do
    try $ symbol "#_"
    form
    return EmptyForm

anonymousFunction = do
    try $ char '#'
    f <- listLiteral

    let symbolIsArgLiteral :: Symbol -> Bool
        symbolIsArgLiteral (Symbol s) = (head s) == '%'

    let collectArgLiterals :: Form -> [Symbol]
        collectArgLiterals (SymbolForm sym) = if symbolIsArgLiteral sym then [sym] else []
        collectArgLiterals _ = []

    let sortArgs :: Symbol -> Symbol -> Ordering
        sortArgs (Symbol "%") _ = LT
        sortArgs _ (Symbol "%") = GT

        -- TODO: Rest arguments need to be renamed (to begin, instead of end, with &) as part of the rewriting
        sortArgs (Symbol "%&") _ = GT
        sortArgs _ (Symbol "%&") = LT
        sortArgs (Symbol a) (Symbol b) =
           compare (read (tail a) :: Int) (read (tail b) :: Int)

    let args = sortBy sortArgs $ foldForm collectArgLiterals f

    -- #(...) => (fn [args] (...))
    return $ List [SymbolForm (Symbol "fn"), Vector (map SymbolForm args), f]

quotedForm = formMacro "'" "quote"
deref = formMacro "@" "deref"
varQuote = formMacro "#'" "var"

formMacro sym name = do
    try $ symbol sym
    f <- form
    return $ List [SymbolForm (Symbol name), f]

-- TODO: regex patterns
-- TODO: syntax-quote (`), unquote (~), unquote-splicing (~@)
