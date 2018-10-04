{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Text.Printf      (printf)
import           Data.Text        (Text, unpack)
import           Data.ByteString  (ByteString)
import           Data.Time.Clock  (getCurrentTime, UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)

import           Crypto.Hash      (Digest, SHA256)
import qualified Crypto.Hash      (hash)

data BlockNumber = BlockNumber Integer -- uint64, min 2 chars
instance Show BlockNumber where
    show (BlockNumber num) =  printf "%02d" num

data Entry = Entry { index      :: BlockNumber
                   , date       :: UTCTime
                   , group_id   :: BlockNumber
                   , process_id :: BlockNumber
                   , content    :: Text -- UTF8, no ; or \n
                   , hash       :: Digest SHA256 -- SHA256 over string representation
                   }

instance Show Entry where
    show (Entry i d g p c h) =
                  show i
        ++ ";" ++ (formatTime defaultTimeLocale "%d.%m.%y-%H:%M:%S" d)
        ++ ";" ++ show g
        ++ ";" ++ show p
        ++ ";" ++ unpack c
        ++ ";" ++ show h

newEntry index date group_id process_id content = Entry {..}
    where hash = Crypto.Hash.hash ("" :: ByteString)

main :: IO ()
main = do
    now <- getCurrentTime
    let entry1 = show $ newEntry (BlockNumber 1) now (BlockNumber 1) (BlockNumber 1) "Foo"
    print entry1
    writeFile "foo.log" entry1
