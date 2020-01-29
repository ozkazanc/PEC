
import Data.List (intercalate)
import Input
import Game
import Deck
import Odds
import Eval

main :: IO ()
main = do
    putStr "\nHow many players will be playing today? (2-9)\n"
    numPlayers <- getNumPlayers "\n"
    playGame numPlayers


playGame :: Int -> IO ()
playGame numPlayers = do

-- Shuffle Deck
    sdeck <- shuffleDeck deck

-- Preflop
    let (hands, prefboard, prefdeck) = deal sdeck numPlayers
    --putStrLn $ "Player hands: " ++ (show $ fmap (mapTuple (fmap transcribeCard)) hands)
    printPlayerHands hands
    printBoardAndDeck prefboard prefdeck
    --printPlayerOdds $ calculatePreFlopOdds hands prefboard prefdeck
    putStrLn "Press 'enter' to see the flop."
    getLine

-- Flop
    let (postfboard, postfdeck) = flop prefboard prefdeck
    printBoardAndDeck postfboard postfdeck
    printPlayerOdds $ calculateTurnOdds hands postfboard postfdeck
    putStrLn "Press 'enter' to see the turn."
    getLine

-- Turn
    let (posttboard, posttdeck) = turn postfboard postfdeck
    printBoardAndDeck posttboard posttdeck
    printPlayerOdds $ calculateRiverOdds hands posttboard posttdeck
    putStrLn "Press 'enter' to see the river."
    getLine

-- River
    let (postrboard, postrdeck) = river posttboard posttdeck
    printBoardAndDeck postrboard postrdeck
    printPlayerOdds $ calculateWinner hands postrboard postrdeck

    again <- playAgain
    getChar
    if again
        then playGame numPlayers
        else putStrLn "Thank you for playing!\n"


printBoardAndDeck :: Board -> Deck -> IO ()
printBoardAndDeck b d = do
                      putStrLn $ "Board: " ++ (show $ fmap transcribeCard b)
                      --putStrLn $ "Deck: " ++ (show $ fmap transcribeCard d)

printPlayerOdds :: [(Int, Odds)] -> IO ()
printPlayerOdds os = putStrLn $ "Player odds: " ++ msg ++ "\n"
                       where msg = intercalate ", " $ fmap (\(x,y) -> (show x) ++ ": " ++ (show y) ++ "%") os

printPlayerHands :: [(Int, [Card])] -> IO ()
printPlayerHands ps = let msg = fmap printHand ps
                      in  putStrLn $ "\nPlayer Hands: " ++ (intercalate ", " msg)

printHand :: (Int, [Card]) -> String
printHand (x,y) = show x ++ "- " ++ (show $ fmap transcribeCard y)


