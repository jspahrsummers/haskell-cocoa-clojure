{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser (forms)
    where

import AST
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ratio
import qualified Data.Set as Set
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

symbolSpecialChar = oneOf "*+!-_?/.%:&"

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
vectorLiteral = VectorLiteral <$> brackets (many form)

mapLiteral =
    let keyValuePair = (\[x,y] -> (x,y)) <$> count 2 form
    in MapLiteral <$> braces (many keyValuePair)

setLiteral = do
    try $ char '#'
    SetLiteral <$> braces (many form)

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
    forms <- list

    let symbolIsArgLiteral :: String -> Bool
        symbolIsArgLiteral s = head s == '%'

    -- Renames %& arguments to &%, to match the rest syntax required by fn
    let renameArg :: String -> String
        renameArg "%&" = "&%"
        renameArg s = s

        renameForm :: Form -> Form
        renameForm (Symbol s) = Symbol $ renameArg s
        renameForm form = form

    let sortArgs :: String -> String -> Ordering
        sortArgs "%" _ = LT
        sortArgs _ "%" = GT

        sortArgs "%&" _ = GT
        sortArgs _ "%&" = LT

        sortArgs a b = compare (read (tail a) :: Int) (read (tail b) :: Int)

    let args = Set.toList $ Set.filter symbolIsArgLiteral $ collectSymbols forms
        renamedArgs = map renameArg $ sortBy sortArgs args

    -- #(...) => (fn [args] (...))
    return $ List [Symbol "fn", VectorLiteral $ map Symbol renamedArgs, mapForm renameForm forms]

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
