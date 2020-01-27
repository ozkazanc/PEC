
import Game
import Deck

main :: IO ()
main = do
    putStr "How many players will be playing today? (2-9)\n"
    numPlayers <- getNumPlayers "\n"
    playGame numPlayers
    putStrLn "Thanks for playing."


playGame :: Int -> IO ()
playGame numPlayers = putStrLn $ "Oh okay " ++ show numPlayers ++ " players"