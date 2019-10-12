{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString               as B
import qualified Data.Attoparsec.ByteString    as P
import           Criterion.Main
import           System.Environment
import           System.IO
import           System.Exit
import           Types
import           Parse
import           Player

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, tickCount] -> do
            P.Done _ m <- P.parse pModule <$> B.readFile path
            playModule m (read tickCount)
            -- defaultMain [bench path $ nfIO (P.parse pModule <$> B.readFile path)]
        _ -> do
            progName <- getProgName
            hPutStrLn stderr ("usage: " ++ progName ++ " input.mod <ticks>")
            exitFailure
