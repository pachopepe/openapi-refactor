module Main where

import OpenApi3.FileUtils (formatOnly, process)
import System.Console.GetOpt
import Data.List
import System.Environment
import System.Exit
import System.IO

data Flags = FormatOnly
           | Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
       [Option ['f'] ["format-only"]       (NoArg FormatOnly)
            "Format only without refactoring"
       ,Option ['h'] ["help"]       (NoArg Help)
            "Print this help message"
       ]

parse :: [String] -> IO ([Flags], [String])
parse argv = 
    case getOpt Permute flags argv of
        (args,fs,[]) -> 
            if length fs /= 2 || Help `elem` args
                then do hPutStrLn stderr (usageInfo header flags)
                        exitSuccess
                else return (nub args, fs)

        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)

        where header = "Usage: openapi-refactor [-fh] <input-file> <output-file>"


main :: IO ()
main = do
    (as, [inputFile, outputFile]) <- getArgs >>= parse
    if FormatOnly `elem` as
    then
        formatOnly inputFile outputFile 
    else
        process inputFile outputFile 

