module Deck where

import KnuthShuffle (knuthShuffle)

type Value = Int
type Suit = Char
type Card = (Value, Suit)
type Deck = [Card]

deck :: Deck
deck = [(x,y) | y <- "HSDC", x <- [2..14]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = knuthShuffle deck



