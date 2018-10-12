{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Blockchain.Util
    ( mCons
    , mLast
    , mapWithPrev
    , foldWithPrev
    ) where

mCons :: Maybe a -> [a] -> [a]
mCons Nothing list = list
mCons (Just x) list = x:list

mLast :: [a] -> Maybe a
mLast [] = Nothing
mLast list = Just $ last list

mapWithPrev :: (Maybe b -> a -> b) -> [a] -> [b]
mapWithPrev fun list = reverse $ foo [] list
    where foo [] (x:xs) = foo [fun Nothing x] xs
          foo (d:ds) (x:xs) = foo ((fun (Just d) x):d:ds) xs
          foo done [] = done

foldWithPrev :: (b -> Maybe a -> a -> b) -> b -> [a] -> b
foldWithPrev _   default' []   = default'
foldWithPrev fun default' list = foo default' Nothing list
    where foo acc _    []     = acc
          foo acc prev (x:xs) = foo (fun acc prev x) (Just x) xs
