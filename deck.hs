module Deck where

import KnuthShuffle (knuthShuffle)

type Value = Int
type Suit = Char
type Card = (Value, Suit)
type Deck = [Card]
type Hand = (Int, [Card])
type Board = [Card]

-- Basic 52 card deck
deck :: Deck
deck = [(x,y) | y <- "HSDC", x <- [2..14]]

-- Shuffles the deck randomly by using knuth-shuffle algorithm
shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = knuthShuffle deck

-- When printing on a screen, it is easier to transcribe it
transcribeCard :: Card -> String
transcribeCard (14,s) = 'A' : [s]
transcribeCard (13,s) = 'K' : [s]
transcribeCard (12,s) = 'Q' : [s]
transcribeCard (11,s) = 'J' : [s]
transcribeCard (v,s) = (show v) ++ [s]



