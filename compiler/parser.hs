{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser (forms)
    where

import AST
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ratio
import Text.Parsec hiding ((<|>), many)
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
integer = P.integer lexer
naturalOrFloat = P.naturalOrFloat lexer

whiteSpace = skipMany (space <|> char ',')

stringLiteral = StringLiteral <$> P.stringLiteral lexer
identifier = Symbol <$> P.identifier lexer

-- TODO: Clojure number parsing rules
-- TODO: BigDecimal (M suffix) support
numberLiteral = do
    let parseDenom = do
            try (symbol "/")
            integer

    e <- naturalOrFloat
    case e of
        (Right dbl) -> return $ DecimalLiteral $ toRational dbl
        (Left num) -> do
            m <- optionMaybe parseDenom

            return $ case m of
                (Just denom) -> DecimalLiteral $ num % denom
                Nothing -> IntegerLiteral num

nil = NilLiteral <$ reserved "nil"
true = BooleanLiteral True <$ reserved "true"
false = BooleanLiteral False <$ reserved "false"

list = List <$> parens (many form)
vectorLiteral = Vector <$> brackets (many form)

mapLiteral =
    let keyValuePair = (\[x,y] -> (x,y)) <$> count 2 form
    in Map <$> braces (many keyValuePair)

setLiteral = do
    try $ char '#'
    Set <$> braces (many form)

form = do
    whiteSpace

    nil <|> true <|> false <|>
        -- reader macros
        ignoreNext <|> quotedForm <|> deref <|> varQuote <|> keyword <|> try anonymousFunction <|>

        numberLiteral <|> stringLiteral <|> characterLiteral <|>
        setLiteral <|> list <|> vectorLiteral <|> mapLiteral <|>

        identifier

forms = many form

-- TODO: Move everything below here into a read table implementation
-- (i.e., they should be reader macros, not hardcoded)

characterLiteral = do
    let newlineLiteral = '\n' <$ symbol "newline"
        spaceLiteral = ' ' <$ symbol "space"
        tabLiteral = '\t' <$ symbol "tab"

    char '\\'
    CharacterLiteral <$>
      choice [ try newlineLiteral
             , try spaceLiteral
             , try tabLiteral
             , anyChar
             ]

ignoreNext = EmptyForm <$ try (symbol "#_") *> form

anonymousFunction = do
    try $ char '#'
    f <- list

    let symbolIsArgLiteral :: String -> Bool
        symbolIsArgLiteral s = head s == '%'

    let collectArgLiterals :: Form -> [Form]
        collectArgLiterals sym@(Symbol s) = [sym | symbolIsArgLiteral s]
        collectArgLiterals _ = []

    let sortArgs :: Form -> Form -> Ordering
        sortArgs (Symbol "%") _ = LT
        sortArgs _ (Symbol "%") = GT

        -- TODO: Rest arguments need to be renamed (to begin, instead of end, with &) as part of the rewriting
        sortArgs (Symbol "%&") _ = GT
        sortArgs _ (Symbol "%&") = LT
        sortArgs (Symbol a) (Symbol b) =
           compare (read (tail a) :: Int) (read (tail b) :: Int)

    let args = sortBy sortArgs $ foldMapForm collectArgLiterals f

    -- #(...) => (fn [args] (...))
    return $ List [Symbol "fn", Vector args, f]

keyword = do
    try $ char ':'
    id <- identifier
    return $ List [Symbol "keyword", id]

quotedForm = formMacro "'" "quote"
deref = formMacro "@" "deref"
varQuote = formMacro "#'" "var"

formMacro sym name = do
    try $ symbol sym
    f <- form
    return $ List [Symbol name, f]

-- TODO: metadata
-- TODO: regex patterns
-- TODO: syntax-quote (`), unquote (~), unquote-splicing (~@)
