module Preprocess where
import Lex
import Data.List
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Control.Monad

type IncludeFile = String

-- "preprocessing" is currently limited to ;#include

{- Helpers -}
fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
init3 :: (a, b, c) -> (a, b)
init3 (a, b, c) = (a, b)
trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c
fsttrd3 :: (a, b1, b2) -> (a, b2)
fsttrd3 (a, b, c) = (a, c)

{- Wrappers -}

contentsToDepsAndToks :: IncludeFile -> String -> IO (IncludeFile, [Token], [IncludeFile])
contentsToDepsAndToks filename contents = case lexDirsAndToks contents of
    Right (dirs, toks) -> case parseDirectives dirs of
        Left err -> throwErr err
        Right deps -> return (filename, toks, deps)
    Left err -> throwErr (show err)
    where throwErr err = hPutStrLn stderr ("lex error: " ++ err) >> exitFailure

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
    where libName = "scm-lib/" ++ name
          localName = name
          throwErr err = hPutStrLn stderr err >> exitFailure

topSortIncludes :: [(IncludeFile, String)] -> IO [(IncludeFile, [Token])]
--                  filename    , contents ->     sorted-list
topSortIncludes = topSortIncludesRec <=< gatherIncludes <=< mapM (uncurry contentsToDepsAndToks)

topSortIncludesRec :: [(IncludeFile, [Token], [IncludeFile])] -> IO [(IncludeFile, [Token])]
topSortIncludesRec [] = return []
topSortIncludesRec nodes
    | null freeNodes = hPutStrLn stderr ("Circular dependency: " ++ show (map fsttrd3 nodes)) >> exitFailure
    | otherwise = do let x = head freeNodes
                     let xFile = fst3 x
                     let rest = filter ((/=xFile) . fst3) nodes
                     rest' <- topSortIncludesRec $ map (\(f, t, d) -> (f, t, delete xFile d)) rest
                     return $ init3 x : rest'
    where freeNodes = filter (null . trd3) nodes

-- recursively lex files and their dependencies
gatherIncludes :: [(IncludeFile, [Token], [IncludeFile])] -> IO [(IncludeFile, [Token], [IncludeFile])]
--                 visited                                ->     filename    , toks   , dependencies
gatherIncludes visited = do
    let unVisited = nub $ concatMap trd3 visited \\ map fst3 visited
    unVisitedContents <- mapM (readFile <=< resolveIncludeFile) unVisited
    if null unVisited then return visited
    else gatherIncludes . (visited++) =<< zipWithM contentsToDepsAndToks unVisited unVisitedContents
