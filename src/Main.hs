{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Data.ByteArray     (convert, append, ByteArray)
import           Data.ByteString    (ByteString)
import           Data.Monoid        ((<>))
import           Data.Text          (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock    (getCurrentTime, UTCTime)
import           Data.Time.Format   (formatTime, defaultTimeLocale)
import           Text.Printf        (printf)

import           Crypto.Hash        (Digest, SHA256)
import qualified Crypto.Hash        (hash)

type BlockNumber = Integer -- uint64, min 2 chars

showBn :: BlockNumber -> String
showBn num =  printf "%02d" num

data Content =
    Content { index      :: BlockNumber
            , date       :: UTCTime
            , group_id   :: BlockNumber
            , process_id :: BlockNumber
            , text       :: Text -- UTF8, no ; or \n
            }

instance Show Content where
    show (Content i d g p t) =
                  showBn i
        ++ ";" ++ (formatTime defaultTimeLocale "%d.%m.%y-%H:%M:%S" d)
        ++ ";" ++ showBn g
        ++ ";" ++ showBn p
        ++ ";" ++ unpack t

data Entry =
    Entry { content :: Content
          , hash    :: Digest SHA256 -- SHA256 over string representation
          }

instance Show Entry where
    show (Entry content h) = show content ++ ";" ++ show h

type Chain = [Entry]

hashContent :: Content -> Digest SHA256
hashContent content = Crypto.Hash.hash . encodeUtf8 . pack $ show content

newEntry :: Content -> Maybe Entry -> Entry
newEntry content Nothing = Entry content (hashContent content)
newEntry content (Just prev) =
    Entry content entryHash
  where
      prevHash    :: ByteString = convert $ hashContent content
      contentHash :: ByteString = convert $ hash prev
      entryHash   = Crypto.Hash.hash $ prevHash <> (encodeUtf8 ";") <> contentHash

mapWithPrev :: (Maybe b -> a -> b) -> [a] -> [b]
mapWithPrev fun list = foo [] list
    where foo [] (x:xs) = foo [fun Nothing x] xs
          foo (d:ds) (x:xs) = foo ((fun (Just d) x):d:ds) xs
          foo done [] = done

main :: IO ()
main = do
    now <- getCurrentTime

    let contents = [ Content 1 now 1 1 "Foo"
                   , Content 1 now 1 1 "Foo"
                   , Content 1 now 1 1 "Foo"
                   , Content 1 now 1 1 "Foo"
                   ]
        chain :: Chain = mapWithPrev
                  (\prev new -> newEntry new prev)
                  contents

    mapM_ (\entry -> appendFile "foo.log" $ show entry ++ "\n") chain
