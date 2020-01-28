module Input where

import Data.Char
import Data.Either

maxPlayers :: Int
maxPlayers = 9

-- Get the number of players playing the game it cannot exceed maxPlayers
getNumPlayers ::  String -> IO Int
getNumPlayers promptAgain = getFromStdin promptAgain getLine isValidNum read

isValidNum :: String -> Either String Bool
isValidNum xs = if isNum xs then let n = read xs
                                 in if n > maxPlayers then Left "Maximum number of players cannot exceed 9.\n"
                                    else if n < 2     then Left "There must be at least 2 players.\n"
                                    else Right True;
                else Left "Please enter a valid number.\n"

isNum :: String -> Bool
isNum [] = False 
isNum xs = all isDigit xs

-- Ask if the user wants to play again; 
-- getYN always returns an uppercase letter, so the check is sufficient
playAgain :: IO Bool
playAgain = do 
  putStr "\nPlay again?(y/n)\n"
  again <- getYN "\n"
  return $ again == 'Y' 
  
getYN :: String -> IO Char
getYN promptAgain = 
  getFromStdin promptAgain getChar isValidYN toUpper

isValidYN :: Char -> Either String Bool
isValidYN c = if c `elem` "ynYN" then 
                                      Right True
                                 else Left "Play again?(y/n)\n"  

-- This contains the logic common to getNum and getYN;
-- it repeatedly prompts until input matching some criteria
-- is given, transforms that input, and returns it
getFromStdin :: String -> (IO a) -> (a -> Either String Bool) -> (a -> b) -> IO b
getFromStdin promptAgain inputF isOk transformOk = do
  input <- inputF
  if isRight $ isOk input
     then return $ transformOk input
     else do
       putStr $ left $ isOk input
       getFromStdin promptAgain inputF isOk transformOk

-- Utilities functions
mapTuple :: (a -> b) -> (c, a) -> (c, b)
mapTuple f (a1, a2) = (a1, f a2)

left :: Either String b -> String
left (Left x) = x