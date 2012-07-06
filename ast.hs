module AST where

newtype Symbol = Symbol String
    deriving Show

data Binding =
    -- TODO: Destructuring
    SymbolBinding Symbol
    deriving Show

data Param =
    PositionalParam Binding |
    RestParam Binding
    deriving Show

data InvokeMethod = InvokeMethod [Param] [Form]
    deriving Show

data Form =
    SymbolForm Symbol |
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

{--
    SpecialFormDef { symbol :: Symbol, init :: Form } |
    SpecialFormIf { test :: Form, thenClause :: Form, elseClause :: Form } |
    SpecialFormDo [Form] |
    SpecialFormLet { bindings :: [(Binding, Form)], exprs :: [Form] } |
    SpecialFormQuote Form |
    SpecialFormVar Symbol |
    SpecialFormFn { name :: Symbol, invokeMethods :: [InvokeMethod] } |
    SpecialFormLoop { bindings :: [(Binding, Form)], exprs :: [Form] } |
    SpecialFormRecur [Form] |
    SpecialFormThrow Form |
    -- TODO: try
    SpecialFormSet Symbol Form

    -- TODO: monitor-enter, monitor-exit?
--}

    deriving Show
