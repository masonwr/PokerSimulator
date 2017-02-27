module Main where

import System.Environment (getArgs)
import Text.Printf (PrintfArg, printf)
import System.Random (getStdRandom, randomR)
import Data.List ((\\), nub)
import PokerData
import PokerRank
import Parser


main :: IO ()
main = do
  args <- getArgs  

  if not (null args)
    then do 
    ifile <- readFile $ head args
    mapM_ processHandString (lines ifile)
    else putStrLn "Usage: ./Main [file]"  


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
  return $ findRank newHand > findRank hand
    

iterrateDiscard :: Deck -> Hand -> Card
                -> Integer -> Integer -> Integer -> IO Double
iterrateDiscard deck hand card iterCount success failExp = if iterCount > 0
    then do
    better <- runSingleDiscard deck hand card
    if better
      then iterrateDiscard deck hand card (iterCount - 1) (success + 1) failExp
      else iterrateDiscard deck hand card (iterCount - 1) success (failExp + 1)
    else return $ success' / iterations

  where success' = fromIntegral success
        iterations = fromIntegral $ failExp + success


runSimulation :: Deck -> Hand -> Card -> IO ()
runSimulation deck hand card = do    
  probImprove <- iterrateDiscard deck hand card 5000 0 0
  putStr $ roundToStr 2 (probImprove * 100) ++ "\t"

runHand :: Hand -> IO ()
runHand hand = do
  putStr $ show (findRank hand) ++ "\t\t"  
  mapM_ (runSimulation standardDeck hand) hand
  putStrLn ""

processHandString :: String -> IO ()
processHandString str = do
  putStr $ str ++ " >>> \t"  
  let hand = validatehand $ handParser str
  case hand of
    Right h -> runHand h
    Left  _ -> putStrLn "Error"

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr n = printf ("%0." ++ show n ++ "f") 

  
