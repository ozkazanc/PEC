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
evaluateOnSecondHighestCard :: [Card] -> [Card] -> Ordering
evaluateOnSecondHighestCard cs os
    | isFlush cs || isStraight cs = EQ
    | otherwise =  res
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

-- Given a 7 card hand, find the 5 best cards that make up the best evaluation and hand
getMaxEval :: [Card] -> (Int, [Card])
getMaxEval hand = let hs = combinations 5 hand
                      bh = last $ sortBy (handCompare) hs
                  in (evaluate bh, bh)

-- First compare 2 hands on their initial evaluations 
-- if they are the same then check for the second highest car evaluation
handCompare :: [Card] -> [Card] -> Ordering
handCompare x y = (evaluate x `compare` evaluate y) `mappend`
                  (evaluateOnSecondHighestCard x y)

-- K items chosen from a list of n items
-- Import the 'tails' function
--   > tails [0,1,2,3]
--   [[0,1,2,3],[1,2,3],[2,3],[3],[]]

-- The implementation first checks if there's no more elements to select,
-- if so, there's only one possible combination, the empty one,
-- otherwise we need to select 'n' elements. Since we don't want to
-- select an element twice, and we want to select elements in order, to
-- avoid combinations which only differ in ordering, we skip some
-- unspecified initial elements with 'tails', and select the next element,
-- also recursively selecting the next 'n-1' element from the rest of the
-- tail, finally consing them together

-- Using list comprehensions
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']