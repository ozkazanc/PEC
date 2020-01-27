
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
    putStrLn $ "Oh okay " ++ show numPlayers ++ " players"
    sdeck <- shuffleDeck deck
    let (hands, board, ssdeck) = deal sdeck numPlayers
    putStrLn $ "Player hands: " ++ (show $ fmap (mapTuple (fmap transcribeCard)) hands)
    putStrLn $ "Board: " ++ (show board)
    putStrLn $ "Deck: " ++ (show $ fmap transcribeCard ssdeck)

--startGame :: Deck -> Int -> IO ()

deal :: Deck -> Int -> ([Hand], Board, Deck)
deal deck n = let (x, d) = splitAt (2*n) deck
              in (zip [1..n] $ chunksOf 2 x, [], d)


chunksOf :: Int -> [Card] -> [[Card]]
chunksOf _ [] = []
chunksOf n xs = take n xs:chunksOf n (drop n xs)

mapTuple :: (a -> b) -> (c, a) -> (c, b)
mapTuple f (a1, a2) = (a1, f a2)