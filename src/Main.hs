{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Data.Maybe         (isJust, catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)
import           Data.Time.Clock    (getCurrentTime)
import           Data.Time.Format   (parseTimeM, defaultTimeLocale)
import           System.Environment (getArgs)

import           Block              ( Chain, Content(..), Block(..)
                                    , newBlock, loadBlock
                                    , validateChain, validateBlock
                                    )
import           Util               (mapWithPrev, mCons, mLast)


-- TODO more efficient implementation
readLastLine :: FilePath -> IO (Maybe Text)
readLastLine = fmap mLast . fmap T.lines . T.readFile

appendBlock :: String -> IO ()
appendBlock filePath = do
    lastLine <- readLastLine filePath
    let lastBlock = lastLine >>= loadBlock
    --now <- getCurrentTime
    now <- parseTimeM False defaultTimeLocale "%d.%m.%y-%H:%M:%S"
                          $ T.unpack "11.10.18-15:00:00"

    let contents = (content <$> lastBlock) `mCons`
                   [ Content 1 now 1 1 "test"
                   , Content 1 now 1 1 "test"
                   ]

    let chain :: Chain = mapWithPrev
                  (\prev new -> newBlock new prev)
                  contents

    print $ validateChain chain

    mapM_ (\entry -> appendFile filePath $ show entry ++ "\n")
          (if isJust lastBlock then tail chain else chain)

checkBlocks :: FilePath -> IO ()
checkBlocks filePath = do
    lines <- T.lines <$> T.readFile filePath
    print lines
    let chain = catMaybes $ map loadBlock lines

    print $ validateBlock (Just $ head chain) (head . tail $ chain)
    print $ length chain
    print $ validateChain chain

main :: IO ()
main = do
    -- Crashes if there no args were provided
    args <- getArgs

    case args of
      ("append":path:_xs) -> appendBlock path
      ("check":path:_xs)  -> checkBlocks path
      _                   -> undefined
