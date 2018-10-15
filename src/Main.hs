{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Control.Monad      (when)
import           Data.Maybe         (isJust, catMaybes)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)
import           Data.Time.Clock    (getCurrentTime)
--import           Data.Time.Format   (parseTimeM, defaultTimeLocale)
import           System.Environment (getArgs)
import           System.Exit        (exitWith, ExitCode(..))
import           System.IO          (hPutStrLn, stderr)

import           Blockchain.Block   ( Chain, Content(..), Block(..)
                                    , newBlock, loadBlock
                                    , validateChain
                                    )
import           Blockchain.Util    (mapWithPrev, mCons, mLast)


-- TODO more efficient implementation
readLastLine :: FilePath -> IO (Maybe Text)
readLastLine = fmap mLast . fmap T.lines . T.readFile

appendBlock :: String -> IO ()
appendBlock filePath = do
    lastLine <- readLastLine filePath
    let lastBlock = lastLine >>= loadBlock
    now <- getCurrentTime
    --now <- parseTimeM False defaultTimeLocale "%d.%m.%y-%H:%M:%S"
    --                      $ T.unpack "11.10.18-15:00:00"

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

checkBlocks :: [Text] -> Either String ()
checkBlocks blockLines
  | (length chain /= length blockLines) =
      Left "Couldn't parse all lines as blocks"
  | (not $ validateChain chain) =
      Left "Block chain is not valid"
  | otherwise = Right ()
  where chain = catMaybes $ map loadBlock blockLines

checkBlocksCmd :: FilePath -> IO ExitCode
checkBlocksCmd filePath = do
    blockLines <- T.lines <$> T.readFile filePath
    case checkBlocks blockLines of
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
