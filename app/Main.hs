module Main where
import System.Random
import Data.List
import Hands
import PokerRank


getRandomElement :: [a] -> IO a
getRandomElement xs = do
  i <- getStdRandom (randomR (1, length xs-1))
  return $ xs !! i


getRandomHand :: IO Hand
getRandomHand = getRandomHand' []
  where getRandomHand' cards = do
          card <- getRandomElement deck
          let hand = card : cards
          if length (nub hand) < 5
            then getRandomHand' hand
            else return $ nub hand


testRank :: (Hand -> Bool) -> IO Hand
testRank rankPred = do
  hand <- getRandomHand
  if rankPred hand
    then return hand
    else testRank rankPred
    

main :: IO ()
main = do
  hand <- getRandomHand
  
  randomDiscard <- getRandomElement hand
  randoCard     <- getRandomElement $ deck \\ hand

  let newHand = randoCard : (hand \\ [randomDiscard])

  putStrLn "old hand:"
  putStrLn $ "discard" ++ show randomDiscard
  putStrLn $ show hand
  putStrLn $ show $ findRank hand

  putStrLn ""

  putStrLn "new hand:"
  putStrLn $ show newHand
  putStrLn $ show $ findRank newHand
  
  
