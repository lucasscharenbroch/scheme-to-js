module Preprocess where
import Lex
import Data.List (nub)
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.IO

type IncludeFile = String

-- "preprocessing" is currently limited to ;#include

{- Directives -}

parseDirectives :: [PPDirective] -> Either String [IncludeFile]
parseDirectives = fmap nub . mapM parseDirective -- (nub ignores duplicates)

parseDirective :: PPDirective -> Either String IncludeFile
parseDirective ["include", name] = Right name
parseDirective ("include":xs) = Left $ "Wrong number of arguments to ;#include ( expected 1; got " ++ show (length xs)  ++ ")"
parseDirective (x:_) = Left $ "Bad preprocessor directive: " ++ x
parseDirective [] = Left "Empty preprocessor directive"

{- Include Files -}

-- find actual filename of include-file
resolveIncludeFile :: String -> IO String
resolveIncludeFile name = do
    isLib <- doesFileExist libName
    isLocal <- doesFileExist localName
    case (isLib, isLocal) of
        (False, False) -> throwErr $ "Can't find included file " ++ name
        (True, False) -> return libName
        (False, True) -> return localName
        (True, True) -> throwErr $ "Included file matches both library and local file: " ++ name
    where libName = "scm-lib/" ++ name ++ ".scm"
          localName = name ++ ".scm"
          throwErr err = hPutStrLn stderr err >> exitFailure
