
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
    putStrLn "Thanks for playing."


playGame :: Int -> IO ()
playGame numPlayers = do

-- Shuffle Deck
    sdeck <- shuffleDeck deck

-- Preflop
    let (hands, prefboard, prefdeck) = deal sdeck numPlayers
    putStrLn $ "Player hands: " ++ (show $ fmap (mapTuple (fmap transcribeCard)) hands)
    printBoardAndDeck prefboard prefdeck
    putStrLn "Press 'enter' to see the flop."
    getLine

-- Flop
    let (postfboard, postfdeck) = flop prefboard prefdeck
    printBoardAndDeck postfboard postfdeck
    putStrLn "Press 'enter' to see the turn."
    getLine

-- Turn
    let (posttboard, posttdeck) = turn postfboard postfdeck
    printBoardAndDeck posttboard posttdeck
    putStrLn "Press 'enter' to see the river."
    getLine

-- River
    let (postrboard, postrdeck) = river posttboard posttdeck
    printBoardAndDeck postrboard postrdeck

    again <- playAgain
    getChar
    if again
        then playGame numPlayers
        else putStrLn "Thank you for playing."


printBoardAndDeck :: Board -> Deck -> IO ()
printBoardAndDeck b d = do
                      putStrLn $ "Board: " ++ (show $ fmap transcribeCard b)
                      --putStrLn $ "Deck: " ++ (show $ fmap transcribeCard d)
