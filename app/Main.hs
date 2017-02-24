module Main where
import System.Random
import Data.List
import Hands
import PokerRank


getRandomElement :: [a] -> IO a
getRandomElement xs = do
  i <- getStdRandom (randomR (1, length xs - 1))
  return $ xs !! i


getRandomHand :: IO Hand
getRandomHand = getRandomHand' []
  where getRandomHand' cards = do
          card <- getRandomElement standardDeck
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
    

runSingleDiscard :: Deck -> Hand -> Card -> IO Bool
runSingleDiscard deck hand discard  = do
  randomCard <- getRandomElement $ deck \\ hand  
  let newHand = randomCard : (hand \\ [discard])
  return $ (findRank newHand) > (findRank hand)
    

iterrateDiscard :: Deck -> Hand -> Card
                -> Integer -> Integer -> Integer -> IO Double
iterrateDiscard deck hand card iterCount success fail = do
  if iterCount > 0
    then do
    better <- runSingleDiscard deck hand card
    if better
      then iterrateDiscard deck hand card (iterCount - 1) (success + 1) fail
      else iterrateDiscard deck hand card (iterCount - 1) success (fail + 1)
    else return $ success' / iterations

  where success' = fromIntegral success
        iterations = fromIntegral $ fail + success



  

main :: IO ()
main = do
  hand <- getRandomHand  
  randomDiscard <- getRandomElement hand

  probImprove <- iterrateDiscard standardDeck hand randomDiscard 1000 0 0

  putStrLn $ "hand: " ++ show hand
  putStrLn $ "probabilyty of improvement with discarding " ++ show randomDiscard
  putStrLn $ show probImprove
