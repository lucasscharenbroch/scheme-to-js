module Main where
import Lex
import Parse
import Generate
import Text.Parsec (ParseError)
import Data.List

-- TODO remove
resToStr :: Either ParseError (Either ParseError [Program]) -> String
resToStr (Left err) = "tokenizing error: " ++ show err
resToStr (Right (Left err)) = "parse error: " ++ show err
resToStr (Right (Right p)) = intercalate "\n" $ map gen p

cases = [
        -- "(())()#(abc abcdef + - + 432.432"
        -- ""
        -- "xyz&&~"
        -- "xyz&&~["
        -- "ab12"
        -- "\"abcd\"123\"efgh\"1.2"
        -- "'abc+123 +123 -123 +.3"
        -- "test words and symbols 1 and ot7er construction (s 9 9) ; comment\nnot a comment"
        -- "test words (and) symbols 1 (and) ot7er construction (s 9 9) ; comment\nnot a comment"
        -- "\\#hi"
        -- "#t #f"
        -- "12ab"
        -- "#true"
        -- "#\\abc"
        -- "#\\a bc"
        -- "(define a b)"
        -- "(define a 3)"
        -- "(((((((((((((((define a 3"
        -- "(((and a b) 1 2"
        -- "("
        -- "(and)"
        -- "(andd)"
        -- "(+ 1 2 (* 3 4))"
        -- "(1 2 ⊆)"
        -- "(((and a b) 1 2)"
        -- "(((and a b) 1 2))"
        -- "()"
        -- "(lambda x 1)"
        -- "(lambda (x y) 1)"
        -- "(lambda (x y) (+ 1 2))"
        -- "(lambda () y)"
        -- "(lambda (x) (define y 2) y)"
        -- "(lambda (x) (define y 2) (define z 3) y)"
        -- "(define (z) 4)"
        -- "(define (z and) 4)"
        -- "(define (z q w) (+ z w))"
        -- "(define (z . q w) (+ z w))"
        -- "(define (z q . w) (define a b) (define (c d) e) (+ z w))"
        -- "(define (z q . w) (+ z w))"
        -- "(define (z . w) (+ z w))"
        -- "(lambda (x) (define y 2) (define (z) 3) y)"
        -- "'(a b c)"
        -- "'()"
        -- "'xyz"
        -- "'123.345"
        -- "'#(vec of things)"
        -- "(list 1 2 3)"
        -- "'(a b c (d e f (1 2 3 ())))"
        -- "(if #t (if #f (+ 1 2) 9) #t)"
        -- "(lambda (x y z) (+ x y z))"
        -- "(define x 2)"
        -- "(define (x) 2)"
        -- "(cond ())"
        -- "(cond)"
        -- "(cond (#t (if #t 123 999)) (#f 456) (xyz) (else 789))"
        -- "(let ((a 1) (b 2) (c 3)) (define d 123) (+ a b c d))"
        -- "(define Abc+- 123)"
        -- "+"
        -- "xyz!"
        -- "1bc"
        -- "'(a b c 1 2 3 #(1 2 3))"
        "(quote (a b c 1 2 3 #(1 2 3)))"
    ]

main = do
    mapM (putStrLn . resToStr . fmap programize . tokenize) cases