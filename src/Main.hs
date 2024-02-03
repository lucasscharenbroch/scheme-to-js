module Main where
import Lex
import Preprocess
import Parse
import Optimize
import Generate
import Text.Parsec (ParseError)
import Data.List
import System.IO
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad

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

parseAndGen :: IncludeFile -> [Token] -> IO String
parseAndGen filename toks = do
    case programize toks of
        Left err -> throwErr $ "parser error: " ++ show err
        Right programs -> return $ intercalate "\n" (map (gen {-. optimize -}) programs)
    where throwErr err = hPutStrLn stderr (filename ++ ": " ++ err) >> exitFailure

validateInfiles :: [String] -> IO ()
validateInfiles [] = hPutStrLn stderr "expected input file (pass \"-\" to read from stdin)" >> exitFailure
validateInfiles _ = return ()

validateOutfile :: [String] -> IO String
validateOutfile [] = return "out.scm.js"
validateOutfile [filename] = return filename
validateOutfile _ = hPutStrLn stderr "too many output files specified (expected 0 or 1)" >> exitFailure

main :: IO ()
main = do
    (isHelp, isVerbose, inputs, outputs) <- sortOptions <$> handleArgv
    when isHelp $ putStr (usageInfo usageHeader optDescriptions) >> exitSuccess
    validateInfiles inputs
    outputFile <- validateOutfile outputs
    inputTexts <- mapM (_readFile isVerbose) inputs
    coreLibraryText <- readFile "js-lib/core.js"
    outputText <- fmap concat $ mapM (uncurry parseAndGen) =<< topSortIncludes (zip inputs inputTexts)
    writeFile outputFile (coreLibraryText ++ outputText)
    where _readFile isVerbose "-" = when isVerbose (putStrLn "reading input from stdin") >> getContents
          _readFile _ name = readFile name
