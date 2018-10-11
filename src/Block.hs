{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Block
    ( newBlock
    , loadBlock
    , validateChain
    , validateBlock
    , Chain
    , Content(..)
    , Block(..)
    ) where

import qualified Data.ByteString.Base16 as Base16
import           Data.Either.Combinators (rightToMaybe)
import           Data.Text          (Text, pack, unpack, breakOnEnd, split)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Read     (decimal)
import           Data.Time.Clock    (UTCTime)
import           Data.Time.Format   (formatTime, parseTimeM, defaultTimeLocale)
import           Text.Printf        (printf)

import           Crypto.Hash        (Digest, SHA256, digestFromByteString)
import qualified Crypto.Hash        (hash)

import           System.IO.Unsafe   (unsafePerformIO)

import           Util               (foldWithPrev)

type BlockNumber = Integer -- uint64, min 2 chars
type Chain = [Block]

showBn :: BlockNumber -> String
showBn num =  printf "%02d" num

-- Delimiter between Content and Block elements
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

data Block =
    Block { content :: Content
          , hash    :: Digest SHA256 -- SHA256 over string representation
          }

instance Show Block where
    show (Block content hash) = show content ++ ";" ++ show hash

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

loadHash :: Text -> Maybe (Digest SHA256)
loadHash = digestFromByteString . fst . Base16.decode . encodeUtf8

loadBlock :: Text -> Maybe Block
loadBlock str = Block <$> (loadContent contentStr)
                      <*> (loadHash hashStr)
    where (contentStr, hashStr) = breakOnEnd del str
          -- TODO if either is empty should return Nothing

hashContent :: Content -> Digest SHA256
hashContent content = unsafePerformIO $ do
        print $ show content
        print contentHash
        return contentHash
            where contentHash :: Digest SHA256 = Crypto.Hash.hash . encodeUtf8 . pack $ show content ++ ";"

hashWithPrev :: Content -> Block -> Digest SHA256
hashWithPrev content prev = Crypto.Hash.hash hashStr
  where
      prevHashHex = encodeUtf8 . pack . show $ hash prev
      contentBytes = encodeUtf8 . pack . show $ content
      hashStr = unsafePerformIO $ do
          print  $ prevHashHex <> (encodeUtf8 del) <> contentBytes <> (encodeUtf8 del)
          return $ prevHashHex <> (encodeUtf8 del) <> contentBytes <> (encodeUtf8 del)

newBlock :: Content -> Maybe Block -> Block
newBlock c Nothing     = Block c (hashContent c)
newBlock c (Just prev) = Block incrementedContent (hashWithPrev incrementedContent prev)
  where
      incrementedContent = c { index = (+1) . index $ content prev }

validateBlock :: Maybe Block -> Block -> Bool
validateBlock Nothing     (Block content hash) = hash == hashContent content
validateBlock (Just prev) (Block content hash) = hash == hashWithPrev content prev

validateChain :: Chain -> Bool
validateChain []     = True
validateChain blocks =
    foldWithPrev (\acc x y -> acc && validateBlock x y) True blocks
