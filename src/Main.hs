module Main where
import Lex
import Parse

cases = [
        "(())()#(abc abcdef + - + 432.432",
        "(",
        "",
        "xyz&&~",
        "xyz&&~[",
        "ab12",
        "\"abcd\"123\"efgh\"1.2",
        "'abc+123 +123 -123 +.3",
        "\\#hi",
        "#t #f",
        "12ab",
        "#true",
        "#\\abc",
        "#\\a bc",
        "(define a 3)"
    ]

main = do
    mapM (print . (programize<$>) . tokenize) cases