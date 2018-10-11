{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)
import           Data.Time.Clock    (getCurrentTime)
import           System.Environment (getArgs)

import           Block              ( Chain, Content(..), Block(..)
                                    , newBlock, loadBlock
                                    )
import           Util               (mapWithPrev, mCons, mLast)


-- TODO more efficient implementation
readLastLine :: FilePath -> IO (Maybe Text)
readLastLine = fmap mLast . fmap T.lines . T.readFile

main :: IO ()
main = do
    -- Crashes if there no args were provided
    logPath <- last <$> getArgs

    lastLine <- readLastLine logPath
    let lastBlock = lastLine >>= loadBlock
    now <- getCurrentTime

    let contents = (content <$> lastBlock) `mCons`
                   [ Content 1 now 1 1 "Foo"
                   , Content 1 now 1 1 "Foo"
                   --, Content 1 now 1 1 "Foo"
                   --, Content 1 now 1 1 "Foo"
                   ]

    let chain :: Chain = mapWithPrev
                  (\prev new -> newBlock new prev)
                  contents

    mapM_ (\entry -> appendFile logPath $ show entry ++ "\n")
          (if isJust lastBlock then tail chain else chain)
