module Odds where

import Data.List
import Deck
import Eval

type Odds = Double

calculateTurnOdds :: [Hand] -> Board -> Deck -> [(Int, Odds)]
calculateTurnOdds ps b d = let abs = fmap (b++) $ combinations 2 d
                               aps = fmap (calculateEvalForAll ps) abs
                               ids = fmap bestHandForABoard aps
                               tis = countOccurences ids
                               odds = fmap (\(x,y) -> (x,(fromIntegral y/(fromIntegral $ length ids)))) tis
                           in  fmap (\(x,y) -> (x, fromIntegral (floor (y*10000))/100)) odds

getBestHand :: Board -> (Int, [Card]) -> (Int, Int, [Card])
getBestHand b (id, hand) = let (x,y) = getMaxEval (b ++ hand)
                           in (id, x, y)

calculateEvalForAll :: [(Int, [Card])] -> Board -> [(Int, Int, [Card])]
calculateEvalForAll ps b = fmap (getBestHand b) ps

bestHandForABoard :: [(Int, Int, [Card])] -> Int
bestHandForABoard ps = let pss = sortBy (\(_,_,x) (_,_,y) -> handCompare x y) ps
                       in (\(x,y,z) -> x) $ last pss

countOccurences :: (Ord a) => [a] -> [(a, Int)]
countOccurences xs = let grouped = groupBy (==) $ sort xs
                     in fmap (\x -> (head x, length x)) grouped

