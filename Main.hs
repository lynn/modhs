{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString               as B
import qualified Data.Attoparsec.ByteString    as P
import           System.Environment
import           System.IO
import           System.Exit
import           Data.Array                     ( (!), elems )
import           Types
import           Parse

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            contents <- B.readFile path
            case P.parse pModule contents of
                P.Fail b ctxs why -> do
                    putStrLn "Fail"
                    print (B.take 20 b)
                    print ctxs
                    print why
                P.Done b m -> do
                    putStrLn "Done"
                    print (B.take 20 b)
                    mapM_ print (sampleInfos m)
                P.Partial _ -> do
                    putStrLn "Partial"
        _ -> do
            progName <- getProgName
            hPutStrLn stderr ("usage: " ++ progName ++ " input.mod")
            exitFailure
