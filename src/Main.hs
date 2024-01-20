module Main where
import Lex
import Parse
import Generate
import Text.Parsec (ParseError)
import Data.List
import System.IO
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

{- CLI Arg Parse -}

usageHeader = "Usage: s2j input-files [options]"

data CliOption = HelpFlag
               | VerboseFlag
               | OutputFlag String
               | InputFlag String

optDescriptions :: [OptDescr CliOption]
optDescriptions = [
        Option "h" ["help"] (NoArg HelpFlag) "print help information",
        Option "v" ["verbose"] (NoArg VerboseFlag) "enable verbosity",
        Option "o" ["output-file"] (ReqArg OutputFlag "filename") "set output file name"
    ]

handleArgv :: IO [CliOption]
handleArgv = do argv <- getArgs
                let (opts, nonOptions, errs) = getOpt (ReturnInOrder InputFlag) optDescriptions argv
                if not (null errs) then hPutStrLn stderr ("encountered error: " ++ head errs) >> exitFailure
                else if not (null nonOptions) then hPutStrLn stderr ("invalid option: " ++ head nonOptions) >> exitFailure
                else return opts

sortOptions :: [CliOption] -> (Bool, Bool, [String], [String])
sortOptions = foldl foldOpts (False, False, [], [])
    where foldOpts (h, v, i, o) HelpFlag = (True, v, i, o)
          foldOpts (h, v, i, o) VerboseFlag = (h, True, i, o)
          foldOpts (h, v, i, o) (InputFlag inFile) = (h, v, i ++ [inFile], o)
          foldOpts (h, v, i, o) (OutputFlag outFile) = (h, v, i, o ++ [outFile])

transpile :: String -> Either String String
transpile input = case tokenize input of
    Left err -> Left $ "Lexing Error: " ++ show err
    Right toks -> case programize toks of
        Left err -> Left $ "Parsing Error: " ++ show err
        Right programs -> Right . intercalate "\n" . map gen $ programs

validateInfiles :: [String] -> IO ()
validateInfiles [] = hPutStrLn stderr "expected input file (pass \"-\" to read from stdin)" >> exitFailure
validateInfiles _ = return ()

validateOutfile :: [String] -> IO String
validateOutfile [] = return "out.scm.js"
validateOutfile [filename] = return filename
validateOutfile _ = hPutStrLn stderr "Too many output files specified (expected 0 or 1)" >> exitFailure

main :: IO ()
main = do
    (isHelp, isVerbose, inputs, outputs) <- sortOptions <$> handleArgv
    if isHelp then putStr $ usageInfo usageHeader optDescriptions
    else main' isVerbose inputs outputs

main' :: Bool -> [String] -> [String] -> IO ()
main' isVerbose inputs outputs = do
    validateInfiles inputs
    outputFile <- validateOutfile outputs
    inputText <- concat <$> mapM _readFile inputs
    case transpile inputText of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right outputText -> writeFile outputFile outputText
    where _readFile "-" = getContents
          _readFile name = readFile name
