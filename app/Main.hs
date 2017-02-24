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


testRank rankPred = do
  hand <- getRandomHand
  if rankPred hand
    then return hand
    else testRank rankPred
    

main :: IO ()
main = do
  randoCard     <- getRandomElement $ deck \\ randoHAnd
  randomDiscard <- getRandomElement randoHAnd

  let newHand = randoCard : (randoHAnd \\ [randomDiscard])

  print randoCard
  print randomDiscard

  print randoHAnd
  print newHand
