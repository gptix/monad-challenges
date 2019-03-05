{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Numeric (readHex)
import Data.List.Split (chunksOf)

hed :: [a] -> a
hed [] = undefined
hed (a:as) = a

hexDecode :: String -> String
hexDecode = map (toEnum . fst . hed . readHex) . (chunksOf 2)

hD = hexDecode
