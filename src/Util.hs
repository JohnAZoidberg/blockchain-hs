{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Util
    ( mCons
    , mLast
    , mapWithPrev
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
