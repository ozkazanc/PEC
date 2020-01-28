
import Input
import Game
import Deck

main :: IO ()
main = do
    putStr "How many players will be playing today? (2-9)\n"
    numPlayers <- getNumPlayers "\n"
    playGame numPlayers
    putStrLn "Thanks for playing."


playGame :: Int -> IO ()
playGame numPlayers = do
    putStrLn $ "\nOh okay " ++ show numPlayers ++ " players"
    sdeck <- shuffleDeck deck
    let (hands, prefboard, prefdeck) = deal sdeck numPlayers
    putStrLn $ "Player hands: " ++ (show $ fmap (mapTuple (fmap transcribeCard)) hands)
    putStrLn $ "Board: " ++ (show $ fmap transcribeCard prefboard)
    putStrLn $ "Deck:  " ++ (show $ fmap transcribeCard prefdeck)
    putStrLn "Press 'enter' to see the flop."
    getLine

    let (postfboard, postfdeck) = flop prefboard prefdeck
    putStrLn $ "Board: " ++ (show $ fmap transcribeCard postfboard)
    putStrLn $ "Deck: " ++ (show $ fmap transcribeCard postfdeck)
    putStrLn "Press 'enter' to see the turn."
    getLine

    let (posttboard, posttdeck) = turn postfboard postfdeck
    putStrLn $ "Board: " ++ (show $ fmap transcribeCard posttboard)
    putStrLn $ "Deck: " ++ (show $ fmap transcribeCard posttdeck)
    putStrLn "Press 'enter' to see the river."
    getLine

    let (postrboard, postrdeck) = river posttboard posttdeck
    putStrLn $ "Board: " ++ (show $ fmap transcribeCard postrboard)
    putStrLn $ "Deck: " ++ (show $ fmap transcribeCard postrdeck)

    again <- playAgain
    getChar
    if again
        then playGame numPlayers
        else putStrLn "Thank you for playing."


