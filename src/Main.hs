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

data Content =
    Content { index      :: BlockNumber
            , date       :: UTCTime
            , group_id   :: BlockNumber
            , process_id :: BlockNumber
            , text       :: Text -- UTF8, no ; or \n
            }

data Entry =
    Entry { content :: Content
          , hash    :: Digest SHA256 -- SHA256 over string representation
          }

instance Show Entry where
    show (Entry (Content i d g p t) h) =
                  show i
        ++ ";" ++ (formatTime defaultTimeLocale "%d.%m.%y-%H:%M:%S" d)
        ++ ";" ++ show g
        ++ ";" ++ show p
        ++ ";" ++ unpack t
        ++ ";" ++ show h

contentHash (Content i d g p t) = Crypto.Hash.hash ("" :: ByteString)

newEntry content Nothing = Entry content (contentHash content)

newEntry content (Just prev) = Entry content entryHash
    where entryHash = Crypto.Hash.hash ("" :: ByteString)

main :: IO ()
main = do
    now <- getCurrentTime

    let entry1 = newEntry (Content (BlockNumber 1)
                                   now
                                   (BlockNumber 1)
                                   (BlockNumber 1)
                                   "Foo")
                          Nothing

    appendFile "foo.log" $ show entry1
    appendFile "foo.log" "\n"
