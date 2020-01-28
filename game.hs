module Game where

import Deck

deal :: Deck -> Int -> ([Hand], Board, Deck)
deal deck n = let (x, d) = splitAt (2*n) deck
              in (zip [1..n] $ chunksOf 2 x, [], d)

flop :: Board -> Deck -> (Board, Deck)
flop board prefdeck = (take 3 pfdeck ++ board, drop 3 pfdeck)
                       where pfdeck = burn prefdeck

turn :: Board -> Deck -> (Board, Deck)
turn board pretdeck = (board ++ take 1 posttdeck, drop 1 posttdeck)
                       where posttdeck = burn pretdeck

river :: Board -> Deck -> (Board, Deck)
river = turn

burn :: Deck -> Deck
burn bdeck = drop 1 bdeck

-- Groups the list n elements. eg. chunksOf 2 [1,2,3,4,5] -> [[1,2],[3,4],[5]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs:chunksOf n (drop n xs)