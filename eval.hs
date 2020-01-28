module Eval where

import Data.List
import Data.Function (on)
import Deck

-- This function evaulates the given 5 card hand and returns an int value of the hand
-- which is calculated by adding the base hand value  plus the high card that was used
-- (Base Hand)    = (value)
-- Invalid hand   = 0
-- High Card      = 15
-- Pair           = 30
-- Two Pair       = 45
-- Triple         = 60
-- Straight       = 75
-- Flush          = 90
-- Full House     = 105
-- Quad           = 120
-- Straight Flush = 135

evaluate :: [Card] -> Int
evaluate cs
    | length cs /= 5                = 0
    | isFlush cs && isStraight cs   = 135 + straightMax
    | isFlush cs                    = 90  + (maximum $ fmap fst cs)
    | isStraight cs                 = 75  + straightMax
    | otherwise                     = pairs cs
    where
        isFiveHigh =
            maximum (fmap fst cs) == 14 &&
            minimum (fmap fst cs) == 2
        straightMax = if isFiveHigh then 5 else maximum (fmap fst cs) 

-- This function is called when 2 hands evaluate to the same result to check the second highes card
evaluateOnSecondHighestCard :: [Card] -> [Card] -> Int
evaluateOnSecondHighestCard cs os
    | isFlush cs || isStraight cs = 0
    | otherwise =  if res == EQ then 0
                   else if res == LT then -1
                   else 1
    where
        vsCS = group $ sort $ fmap fst cs
        vsOS = group $ sort $ fmap fst os
        sCS = sortBy (compare `on` length) vsCS
        sOS = sortBy (compare `on` length) vsOS
        res = compare (reverse sCS) (reverse sOS)


isFlush :: [Card] -> Bool
isFlush hand = all (==h) $ fmap snd hand
               where h = snd $ head hand

isStraight :: [Card] -> Bool
isStraight cs =
    vs == [minimum vs..maximum vs] ||
    maximum vs == 14 &&
    init vs == [2,3,4,5]
    where vs = sort $ fmap fst cs

-- We sort the the given hand by their values and then group them to get how many of them there are
pairs :: [Card] -> Int
pairs cs = case lengths of
    [1,4]       -> 120 + maxCard
    [2,3]       -> 105 + maxCard
    [1,1,3]     -> 60  + maxCard
    [1,2,2]     -> 45  + maxCard
    [1,1,1,2]   -> 30  + maxCard
    [1,1,1,1,1] -> 15  + maxCard
    _           -> 0
    where
        sorted = sortBy (\x y -> if fst x < fst y then LT else GT) cs
        grouped = groupBy (\x y -> fst x == fst y) sorted
        lengths = sort $ fmap length grouped
        maxCard =  fst $ (last $ sortBy (\x y -> if length x <= length y then LT else GT) grouped) !! 0
