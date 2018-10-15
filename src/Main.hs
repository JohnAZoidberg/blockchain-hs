{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Data.Maybe         (isJust, catMaybes, maybeToList)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)
import           Data.Time.Clock    (getCurrentTime)
--import           Data.Time.Format   (parseTimeM, defaultTimeLocale)
import           System.Environment (getArgs)
import           System.Exit        (exitWith, ExitCode(..))
import           System.IO          (hPutStrLn, stderr)

import           Blockchain.Block   ( Chain, Content(..)
                                    , newBlock, loadBlock
                                    , validateChain, validateTextChain
                                    )
import           Blockchain.Util    (mapWithPrev, mLast)


-- TODO more efficient implementation
readLastLine :: FilePath -> IO (Maybe Text)
readLastLine = fmap mLast . fmap T.lines . T.readFile

appendBlockCmd :: String -> IO ()
appendBlockCmd filePath = do
    lastLine <- readLastLine filePath
    let lastBlock = lastLine >>= loadBlock

    now <- getCurrentTime
    --now <- parseTimeM False defaultTimeLocale "%d.%m.%y-%H:%M:%S"
    --                      $ T.unpack "11.10.18-15:00:00"

    print $ lastBlock

    let contents = [ Content 1 now 1 1 "test"
                   , Content 1 now 1 1 "test"
                   ]

    let chain :: Chain = mapWithPrev (maybeToList lastBlock)
                  (\prev new -> newBlock new prev)
                  contents

    mapM_ (\entry -> appendFile filePath $ show entry ++ "\n")
          (if isJust lastBlock then tail chain else chain)

checkBlocksCmd :: FilePath -> IO ExitCode
checkBlocksCmd filePath = do
    blockLines <- T.lines <$> T.readFile filePath
    case validateTextChain blockLines of
      Left err -> do
          hPutStrLn stderr err
          return $ ExitFailure 1
      Right () ->
          return ExitSuccess

main :: IO ()
main = do
    -- Crashes if there no args were provided
    args <- getArgs

    case args of
      ("append":path:_xs) -> appendBlockCmd path
      ("check":path:_xs)  -> checkBlocksCmd path >>= exitWith
      _                   -> undefined
