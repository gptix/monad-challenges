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



allPairs :: [a] -> [b] -> [(a,b)]
allPairs _ [] = []
allPairs [] _ = []
allPairs as bs = concat $ map (\y -> ( map (\x -> (y,x)) bs)) as

data Card = Card Int String

instance Show Card where
  show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards _ [] = []
allCards [] _ = []
allCards vals suits = concat $ map (\val -> ( map (\suit -> Card val suit) suits)) vals
  
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ _ [] = []
allCombs _ [] _ = []
allCombs f as bs = concat $ map (\a -> (map (\b -> f a b) bs)) as

allPairs' :: [a] -> [b] -> [(a,b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card 

allCombs3 :: (a -> (b -> (c -> d))) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ _ _  [] = []
allCombs3 _ _ [] _  = []
allCombs3 _ [] _ _  = []
allCombs3 f as bs cs  = concat $ map (\a -> concat $ (map (\b -> (map (\c -> f a b c) cs)) bs)) as

combStep :: [a -> b] -> [a] -> [b]
combStep fs as = concat $ map (\f -> map (\a -> f a) as) fs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f as bs = map f  as `combStep` bs

allCombs3' :: (a -> (b -> (c -> d))) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs cs = map f as `combStep` bs `combStep` cs
