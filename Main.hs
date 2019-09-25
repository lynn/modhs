{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString               as B
import qualified Data.Attoparsec.ByteString    as P
import           Criterion.Main
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
            defaultMain [bench path $ nfIO (P.parse pModule <$> B.readFile path)]
        _ -> do
            progName <- getProgName
            hPutStrLn stderr ("usage: " ++ progName ++ " input.mod")
            exitFailure
