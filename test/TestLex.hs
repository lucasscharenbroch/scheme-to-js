module TestLex where
import Lex
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

allLexTests :: TestTree
allLexTests = testCase "lex tests" $ intercalate "\n" expected @?= (intercalate "\n" . map (show . tokenize) $ cases)


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
        "#\abc"
    ]

expected = [
    "Right [TokOpen,TokOpen,TokClose,TokClose,TokOpen,TokClose,TokHashOpen,TokId \"abc\",TokId \"abcdef\",TokId \"+\",TokId \"-\",TokId \"+\",TokNum 432.432]",
    "Right [TokOpen]",
    "Right []",
    "Right [TokId \"xyz&&~\"]",
    "Right [TokId \"xyz&&~\"]",
    "Right [TokId \"ab12\"]",
    "Right [TokString \"abcd\",TokNum 123.0,TokString \"efgh\",TokNum 1.2]",
    "Right [TokQuote,TokId \"abc+123\",TokId \"+\",TokNum 123.0,TokId \"-\",TokNum 123.0,TokId \"+\",TokNum 0.3]",
    "Right []",
    "Right [TokBool True,TokBool False]",
    "Left (line 1, column 3):\nunexpected 'a'\nexpecting digit, \".\", space or end of input",
    "Left (line 1, column 3):\nunexpected 'r'\nexpecting space or end of input",
    "Right []"
    ]