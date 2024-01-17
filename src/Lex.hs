module Lex where

import Text.Parsec
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

{-

Lexer Grammar
(from https://standards.scheme.org/official/r3rs.pdf)

token -> identifier | boolean | number
       | character | string | ( | ) | #( | ' | .

delimeter -> whitespace | ( | ) | " | ;

whitespace -> space | newline

comment -> ; <ignore all subsequent chars until newline>

atmosphere -> whitespace | comment

intertoken_space -> atmosphere*

identifier -> initial subsequent* | pecuilar_identifier

initial -> letter | special_initial

letter -> a | b | ... | z

special_initial -> ! | $ | % | & | * | / | : | < | = 
                 | > | ? | ~ | _ | ^

subsequent -> initial | digit | special_subsequent

digit -> 0 | 1 | ... | 9

special_subsequent -> . | + | -

pecuilar_identifier -> + | -

syntactic_keyword -> expression_keyword | else | define

expression_keyword -> quote | lambda | if | set!
                    | begin | cond | and | or | case
                    | let | let* | letrec | do

variable -> <identifier, which isn't also a syntactic_keyword>

boolean -> #t | #f

character -> #\ <char> | #\ character_name

character_name -> space | newline

string -> " string_element* "

string_element -> <non-"-char> | \" | \\

number -> [sign] digit+ [. digit*]
        | [sign] . digit+

-}

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

data ArithSign = Positive | Negative

type LexFn = Parsec String ()

{- Lex Functions -}


-- token -> identifier | boolean | number
--        | character | string | ( | ) | #( | ' | .
-- 
-- delimeter -> whitespace | ( | ) | " | ;
-- 
-- whitespace -> space | newline
-- 
-- comment -> ; <ignore all subsequent chars until newline>
-- 
-- atmosphere -> whitespace | comment
-- 
-- intertoken_space -> atmosphere*
-- 
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
-- 
-- syntactic_keyword -> expression_keyword | else | define
-- 
-- expression_keyword -> quote | lambda | if | set!
--                     | begin | cond | and | or | case
--                     | let | let* | letrec | do
-- 
-- variable -> <identifier, which isn't also a syntactic_keyword>
-- 
-- boolean -> #t | #f
-- 
-- character -> #\ <char> | #\ character_name
-- 
-- character_name -> space | newline
-- 
-- string -> " string_element* "
-- 
-- string_element -> <non-"-char> | \" | \\
--


-- number -> [sign] digit+ [. digit*]
--         | [sign] . digit+

lexNumber :: LexFn Double
lexNumber = do sign <- lexSign
               leftDigits <- many digit
               let lexDecDigits = if length leftDigits == 0 then many1 digit else many digit
               decimalPart <- (string "." <> lexDecDigits) <|> return ""
               return . strToD' $ leftDigits ++ decimalPart
    where strToD' ('+':s) = read s
          strToD' s = read s

-- sign -> + | - | <nothing>

lexSign :: LexFn Char
lexSign = char '+'
      <|> char '-'
      <|> return '+' 