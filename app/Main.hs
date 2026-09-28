-- | Smoke-test CLI: reads and evaluates a file of Scheme source,
-- printing each top-level result. Demonstrates the embedding API end to
-- end - a host does exactly this (@newGlobalEnv@, then @interpret@).
module Main (main) where

import Control.Exception (displayException, try)
import Data.Text.IO qualified as TIO
import Language.Scheme
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            src <- TIO.readFile path
            env <- newGlobalEnv
            result <- try (interpret env src)
            case result of
                Left err -> putStrLn (displayException (err :: SchemeError)) *> exitFailure
                Right values -> mapM_ print (filter (/= Unspecified) values)
        _ -> putStrLn "usage: r5rs-hs-parse <file.scm>" *> exitFailure
