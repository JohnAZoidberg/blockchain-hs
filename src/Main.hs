{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Data.ByteArray     (convert)
import qualified Data.ByteString.Base16 as Base16
import           Data.Either.Combinators (rightToMaybe)
import           Data.ByteString    (ByteString)
import           Data.Monoid        ((<>))
import           Data.Text          (Text, pack, unpack, breakOnEnd, split)
import qualified Data.Text          as T
import           Data.Text.IO       as T (readFile)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Read     (decimal)
import           Data.Time.Clock    (getCurrentTime, UTCTime)
import           Data.Time.Format   (formatTime, parseTimeM, defaultTimeLocale)
import           Text.Printf        (printf)
import           System.Environment (getArgs)

import           Crypto.Hash        (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash        (hash)

-- TODO
-- convert every String to Text
-- use ByteArray instead of Digest SHA256 in Entry

type BlockNumber = Integer -- uint64, min 2 chars

showBn :: BlockNumber -> String
showBn num =  printf "%02d" num

-- Delimiter between Content and Entry elements
del :: Text
del = ";"

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
    show (Entry content hash) = show content ++ ";" ++ show hash

maybeTextInteger :: Text -> Maybe Integer
maybeTextInteger txt = fst <$> (rightToMaybe $ decimal txt)

-- TODO proper parser
loadContent :: Text -> Maybe Content
loadContent str = if valid
                     then Content <$> index
                                  <*> date
                                  <*> group_id
                                  <*> process_id
                                  <*> Just text
                     else Nothing
    where entries = split (== ';') str
          valid = length entries == 6
               && T.all (/= '\n') text
          (i:d:g:p:text:_semi:[]) = entries
          index = maybeTextInteger i
          date = parseTimeM False defaultTimeLocale "%d.%m.%y-%H:%M:%S"
                          $ unpack d
          group_id = maybeTextInteger g
          process_id = maybeTextInteger p

--loadHash :: Text -> Maybe (Digest a)
loadHash = digestFromByteString . fst . Base16.decode . encodeUtf8

loadEntry :: Text -> Maybe Entry
loadEntry str = Entry <$> (loadContent contentStr)
                      <*> (loadHash hashStr)
    where (contentStr, hashStr) = breakOnEnd del str
          -- TODO if either is empty should return Nothing

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
      entryHash   = Crypto.Hash.hash $ prevHash <> (encodeUtf8 del) <> contentHash

mapWithPrev :: (Maybe b -> a -> b) -> [a] -> [b]
mapWithPrev fun list = reverse $ foo [] list
    where foo [] (x:xs) = foo [fun Nothing x] xs
          foo (d:ds) (x:xs) = foo ((fun (Just d) x):d:ds) xs
          foo done [] = done

-- TODO more efficient implementation
readLastLine :: FilePath -> IO (Maybe Text)
readLastLine = fmap mLast . fmap T.lines . T.readFile

mCons :: Maybe a -> [a] -> [a]
mCons Nothing list = list
mCons (Just x) list = x:list

mLast :: [a] -> Maybe a
mLast [] = Nothing
mLast list = Just $ last list

main :: IO ()
main = do
    -- Crashes if there no args were provided
    logPath <- last <$> getArgs

    lastLine <- readLastLine logPath
    let lastEntry = lastLine >>= loadEntry
    now <- getCurrentTime

    let contents = (content <$> lastEntry) `mCons`
                   [ Content 1 now 1 1 "Foo"
                   --, Content 1 now 1 1 "Foo"
                   --, Content 1 now 1 1 "Foo"
                   --, Content 1 now 1 1 "Foo"
                   ]

    let chain :: Chain = mapWithPrev
                  (\prev new -> newEntry new prev)
                  contents

    mapM_ (\entry -> appendFile logPath $ show entry ++ "\n")
          (if length chain == 1 then chain else tail chain)
