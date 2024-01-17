{-# LANGUAGE FlexibleContexts #-}

module Lex where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

{- Helpers -}

-- in the newer version of Text.Parsec.Char
string' :: Stream s m Char => String -> ParsecT s u m String
string' s = try (string s)

{- Lexer Grammar -}
{- (from https://standards.scheme.org/official/r3rs.pdf, pp. 30-31) -}

{- Data Structures/ Types -}

data Token = TokId String
           | TokBool Bool
           | TokNum Double
           | TokChar Char
           | TokString String
           | TokOpen -- (
           | TokClose -- )
           | TokHashOpen -- #(
           | TokQuote
           | TokPeriod
      deriving (Show)

data ArithSign = Positive | Negative

type LexFn = Parsec String ()

{- Lex Functions -}

tokenize :: String -> Either ParseError [Token]
tokenize = parse lexTokenStream ""

-- token_stream -> (token intertoken_space)* eof
--
-- intertoken_space -> atmosphere*
-- 
-- comment -> ; <ignore all subsequent chars until newline>
-- 
-- atmosphere -> space | newline | comment

lexTokenStream :: LexFn [Token]
lexTokenStream = many (lexToken <* lexIntertokenSpace)
    where lexIntertokenSpace = many (void space <|> lexComment)
          lexComment = void (char ';' *> many (noneOf "\n") <* newline)

-- token -> identifier | boolean | number
--        | character | string | ( | ) | #( | ' | .

lexToken :: LexFn Token
lexToken = TokId <$> try lexIdentifier
       <|> TokBool <$> try lexBoolean
       <|> TokNum <$> try lexNumber
       <|> TokChar <$> try lexCharacter
       <|> TokString <$> try lexString
       <|> TokOpen <$ char '('
       <|> TokClose <$ char ')'
       <|> TokHashOpen <$ string "#("
       <|> TokQuote <$ char '\''
       <|> TokPeriod <$ char '.'

-- identifier -> initial subsequent* | pecuilar_identifier
-- 
-- initial -> letter | special_initial
-- 
-- letter -> a | b | ... | z
-- 
-- special_initial -> ! | $ | % | & | * | / | : | < | = 
--                  | > | ? | ~ | _ | ^
-- 
-- subsequent -> initial | digit | special_subsequent
-- 
-- digit -> 0 | 1 | ... | 9
-- 
-- special_subsequent -> . | + | -
-- 
-- pecuilar_identifier -> + | -

lexIdentifier :: LexFn String
lexIdentifier = (:) <$> lexInitial <*> many lexSubsequent
            <|> (string "+" <|> string "-")
      where lexInitial = letter <|> oneOf "!$%&*/:<=>?~_^"
            lexSubsequent = lexInitial <|> digit <|> oneOf ".+-"

-- boolean -> #t | #f

lexBoolean :: LexFn Bool
lexBoolean = True <$ string' "#t"
         <|> False <$ string' "#f"

-- character -> #\ <char> | #\ character_name
--
-- character_name -> space | newline

lexCharacter :: LexFn Char
lexCharacter = string "#\\" *> (lexCharName <|> anyChar)
      where lexCharName = ' ' <$ string' "space"
                      <|> '\n' <$ string' "newline"


-- string -> " string_element* "
--
-- string_element -> <non-"-or-\-char> | \" | \\

lexString :: LexFn String
lexString = between (char '"') (char '"') (many lexStrElem)
      where lexStrElem  = char '\\' *> (char '\\' <|> char '"')
                      <|> noneOf "\"\\"

-- number -> [sign] digit+ [. digit*]
--         | [sign] . digit+
--
-- sign -> + | - | <nothing>

lexNumber :: LexFn Double
lexNumber = strToD' <$> lexSign <> (many1 digit <> option "" (string "." <> many digit) <|>
                                    string "." <> many1 digit)
    where lexSign = string "+" <|> string "-" <|> return "+"
          strToD' ('+':s) = strToD' s
          strToD' ('-':s) = -1 * strToD' s
          strToD' ('.':s) = read ("0." ++ s)
          strToD' s = read s