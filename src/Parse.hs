module Parse where
import Lex
import Text.Parsec
import Text.Parsec.Prim

{- Constants -}

keywords =  [
        "quote", "lambda", "if", "set!", "begin", "cond",
        "and", "or", "let", "let*", "letrec", "define"
    ]

{- Data Structures -}

data Expression = ExprVar String
                | ExprNumber Double
                | ExprChar Char
                | ExprString String
                | ExprBool Bool
                | ExprQuotation Datum
                | ExprProcedureCall Expression [Expression]
                | ExprTailCall [String] [Expression] -- produced during optimization
                | ExprList [Expression]              -- produced during optimization (forming args for varadic tail call)
                | ExprLambda FormalArgs Body
                -- (special forms)
                | ExprIf Expression Expression Expression
                | ExprAssignment String Expression
                | ExprCond [CondClause]
                | ExprAnd [Expression]
                | ExprOr [Expression]
                | ExprLet [Definition] Body
                | ExprLetStar [Definition] Body
                | ExprLetRec [Definition] Body
                | ExprBegin [Expression]
    deriving (Show)

data CondClause = CondIf Expression [Expression]
                | CondElse [Expression]
    deriving (Show)

-- Datum: similar to Expression, but without special forms
-- (used for quotation)
data Datum = DatumSymbol String
           | DatumQuotation Datum
           | DatumBool Bool
           | DatumNumber Double
           | DatumChar Char
           | DatumString String
           | DatumPair Datum Datum
           | DatumNull
           | DatumVector [Datum]
    deriving (Show)

-- function body
data Body = Body [Definition] [Expression]
    deriving (Show)

data FormalArgs = FormalArgList [String]
                | FormalVarArgs [String] String
    deriving (Show)

-- (define x y), (define (x ...) body), or (define (x ... . y) body)
data Definition = DefSimple String Expression
                | DefFunction String FormalArgs Body
                | DefTailRecFunction String FormalArgs Body
    deriving (Show)

data Program = ProgDefinition Definition
             | ProgExpression Expression
    deriving (Show)

type ParseFn = Parsec [Token] ()

{- High-Level Wrappers -}

programize :: [Token] -> Either ParseError [Program]
programize = parse parseProgramStream ""

parseProgramStream :: ParseFn [Program]
parseProgramStream = many parseProgram <* eof

{- Parse Functions -}

{- Token Primitives/ Related Helpers -}

inParens :: ParseFn a -> ParseFn a
inParens = between parseOpen parseClose

updatePosTok :: SourcePos -> Token -> [Token] -> SourcePos
updatePosTok _ _ (tok:_) = getTokSource tok
updatePosTok _ tok' [] =  getTokSource tok'

parseIdentifier :: ParseFn String
parseIdentifier = tokenPrim show updatePosTok (getId . getTokData)
    where getId (TokId id) = Just id
          getId _ = Nothing

parseLitId :: String -> ParseFn String
parseLitId s = try parse'
    where parse' = do id <- parseIdentifier
                      if id == s
                      then return s
                      else parserFail $ "failed to match `" ++ s ++ "`"

parseBoolean :: ParseFn Bool
parseBoolean = tokenPrim show updatePosTok (getBool . getTokData)
    where getBool (TokBool b) = Just b
          getBool _ = Nothing

parseNumber :: ParseFn Double
parseNumber = tokenPrim show updatePosTok (getNum . getTokData)
    where getNum (TokNum n) = Just n
          getNum _ = Nothing

parseChar :: ParseFn Char
parseChar = tokenPrim show updatePosTok (getChar . getTokData)
    where getChar (TokChar c) = Just c
          getChar _ = Nothing

parseString :: ParseFn String
parseString = tokenPrim show updatePosTok (getString . getTokData)
    where getString (TokString s) = Just s
          getString _ = Nothing

parseOpen :: ParseFn ()
parseOpen = tokenPrim show updatePosTok (matchOpen . getTokData)
    where matchOpen TokOpen = Just ()
          matchOpen _ = Nothing

parseClose :: ParseFn ()
parseClose = tokenPrim show updatePosTok (matchClose . getTokData)
    where matchClose TokClose = Just ()
          matchClose _ = Nothing

parseHashOpen :: ParseFn ()
parseHashOpen = tokenPrim show updatePosTok (matchHashOpen . getTokData)
    where matchHashOpen TokHashOpen = Just ()
          matchHashOpen _ = Nothing

parseQuote :: ParseFn ()
parseQuote = tokenPrim show updatePosTok (matchQuote . getTokData)
    where matchQuote TokQuote = Just ()
          matchQuote _ = Nothing

parsePeriod :: ParseFn ()
parsePeriod = tokenPrim show updatePosTok (matchPeriod . getTokData)
    where matchPeriod TokPeriod = Just ()
          matchPeriod _ = Nothing

{- Grammar -}

-- expression -> expression_primitive
--             | ( expression_parenthetical )

parseExpression :: ParseFn Expression
parseExpression = parseExpressionPrimitive
              <|> inParens parseExpressionParenthetical
              <?> "expression"

-- expression_primitive -> boolean
--                       | number
--                       | character
--                       | string
--                       | quotation

parseExpressionPrimitive :: ParseFn Expression
parseExpressionPrimitive = ExprVar <$> parseVariable
              <|> ExprBool <$> parseBoolean
              <|> ExprNumber <$> parseNumber
              <|> ExprChar <$> parseChar
              <|> ExprString <$> parseString
              <|> parseQuotation
              <?> "primitive (non-parenthesized) expression"

-- expression_parenthetical -> quotation_alt
--                           | procedure_call
--                           | lambda
--                           | special_form

parseExpressionParenthetical :: ParseFn Expression
parseExpressionParenthetical = parseQuotationAlt
                           <|> parseProcedureCall
                           <|> parseLambda
                           <|> parseSpecialForm
                           <?> "parenthesized expression (procedure / lambda / special form)"

-- variable -> <non-keyword-id>

parseVariable :: ParseFn String
parseVariable = try parseVariable'
    where parseVariable' = do id <- parseIdentifier
                              if id `elem` keywords
                              then parserFail $ "bad variable (keyword): `" ++  id ++ "`"
                              else return id

-- quotation -> ' datum

parseQuotation :: ParseFn Expression
parseQuotation = ExprQuotation <$> (parseQuote *> parseDatum)

-- quotation_alt -> quote datum

parseQuotationAlt :: ParseFn Expression
parseQuotationAlt = ExprQuotation <$> (parseLitId "quote" *> parseDatum)

-- procedure_call -> expression expression*

parseProcedureCall :: ParseFn Expression
parseProcedureCall = ExprProcedureCall <$> parseExpression <*> many parseExpression

-- lambda -> lambda formals body
-- 
-- formals -> variable
--          | (variable+ . variable)
--          | ( variable* )

parseLambda :: ParseFn Expression
parseLambda = parseLitId "lambda" *> (ExprLambda <$> parseFormals <*> parseBody)
    where parseFormals = FormalVarArgs [] <$> parseVariable
                     <|> inParens parseFormals'
          parseFormals' = try (FormalVarArgs <$> many1 parseVariable <*> (parsePeriod *> parseVariable))
                      <|> FormalArgList <$> many parseVariable

-- body -> definition* sequence

parseBody :: ParseFn Body
parseBody = Body <$> many (try $ inParens parseDefinition) <*> many1 parseExpression

-- special_form -> sf_if
--               | sf_set_bang
--               | sf_cond
--               | sf_and
--               | sf_or
--               | sf_let
--               | sf_let*
--               | sf_letrec
--               | sf_begin
--               | sf_quote

parseSpecialForm :: ParseFn Expression
parseSpecialForm = parseSfIf
               <|> parseSfSetBang
               <|> parseSfCond
               <|> parseSfAnd
               <|> parseSfOr
               <|> parseSfLet
               <|> parseSfLetStar
               <|> parseSfLetRec
               <|> parseSfBegin
               <|> parseSfQuote

-- sf_if -> if expression expression expression

parseSfIf :: ParseFn Expression
parseSfIf = ExprIf <$> (parseLitId "if" *> parseExpression)
                   <*> parseExpression
                   <*> parseExpression

-- sf_set_bang -> set! variable expression

parseSfSetBang :: ParseFn Expression
parseSfSetBang = ExprAssignment <$> (parseLitId "set!" *> parseVariable)
                                <*> parseExpression

-- sf_cond -> cond cond_clause+
--          | cond cond_clause* ( else sequence )
--
-- cond_clause -> ( expression sequence )

parseSfCond :: ParseFn Expression
parseSfCond = ExprCond <$> (parseLitId "cond" *> parseInner)
    where parseInner = do clauses <- many parseCondClause
                          elseClause <- option [] ((:[]) <$> parseElseClause)
                          case clauses ++ elseClause of
                              [] -> parserFail "empty cond statement"
                              clauses' -> return clauses'
          parseCondClause = try $ inParens $ CondIf <$> (notFollowedBy (parseLitId "else") *> parseExpression) <*> many parseExpression
          parseElseClause = inParens $ CondElse <$> (parseLitId "else" *> many parseExpression)

-- sf_and -> and expression*

parseSfAnd :: ParseFn Expression
parseSfAnd = ExprAnd <$> (parseLitId "and" *> many parseExpression)

-- sf_or -> or expression*

parseSfOr :: ParseFn Expression
parseSfOr = ExprOr <$> (parseLitId "or" *> many parseExpression)

-- sf_let -> let ( binding_spec* ) body

parseSfLet :: ParseFn Expression
parseSfLet = ExprLet <$> (parseLitId "let" *> inParens (many parseBindingSpec)) <*> parseBody

-- sf_let* -> let* ( binding_spec* ) body

parseSfLetStar :: ParseFn Expression
parseSfLetStar = ExprLetStar <$> (parseLitId "let*" *> inParens (many parseBindingSpec)) <*> parseBody

-- sf_letrec -> letrec ( binding_spec* ) body

parseSfLetRec :: ParseFn Expression
parseSfLetRec = ExprLetRec <$> (parseLitId "letrec" *> inParens (many parseBindingSpec)) <*> parseBody

-- binding_spec -> ( variable expression)

parseBindingSpec :: ParseFn Definition
parseBindingSpec = inParens $ DefSimple <$> parseVariable <*> parseExpression

-- sf_begin -> begin seuqence

parseSfBegin :: ParseFn Expression
parseSfBegin = ExprBegin <$> (parseLitId "begin" *> many parseExpression)

-- sf_quote -> quote datum

parseSfQuote :: ParseFn Expression
parseSfQuote = ExprQuotation <$> (parseLitId "quote" *> parseDatum)

-- datum -> boolean
--        | number
--        | character
--        | string
--        | symbol
--        | list
--        | vector

parseDatum :: ParseFn Datum
parseDatum = DatumBool <$> parseBoolean
         <|> DatumNumber <$> parseNumber
         <|> DatumChar <$> parseChar
         <|> DatumString <$> parseString
         <|> DatumSymbol <$> parseIdentifier
         <|> DatumVector <$> parseVector
         <|> DatumQuotation <$> (parseQuote *> parseDatum)
         <|> parseList

-- list -> ( datum* )

parseList :: ParseFn Datum
parseList = DatumNull <$ try (parseOpen *> parseClose)
        <|> inParens ((unlistify . reverse) .: reverseCons <$> many1 parseDatum <*> option DatumNull (parsePeriod *> parseDatum))
    where reverseCons :: [Datum] -> Datum -> [Datum]
          reverseCons xs x = x : reverse xs
          unlistify :: [Datum] -> Datum
          unlistify [d] = d
          unlistify (d:ds) = DatumPair d (unlistify ds)
          (.:) = (.) . (.)

-- vector -> #( datum* )

parseVector :: ParseFn [Datum]
parseVector = between parseHashOpen parseClose (many parseDatum)

-- program -> expression | definition

parseProgram :: ParseFn Program
parseProgram = ProgExpression <$> parseExpressionPrimitive
           <|> inParens (ProgExpression <$> parseExpressionParenthetical <|>
                         ProgDefinition <$> parseDefinition)
           <?> "expression or definition"

-- definition -> define variable expression
--             | define ( variable variable* ) body
--             | define ( variable variable* . variable ) body
--
-- def_formals -> variable* . variable
--              | variable*

parseDefinition :: ParseFn Definition
parseDefinition = parseLitId "define" *> parseInner
    where parseInner = DefSimple <$> parseVariable <*> parseExpression
                   <|> inParens (DefFunction <$> parseVariable <*> parseFormals) <*> parseBody
          parseFormals = try (FormalVarArgs <$> many parseVariable <*> (parsePeriod *> parseVariable))
                     <|> FormalArgList <$> many parseVariable
