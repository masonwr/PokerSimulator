module PokerRank where

import Data.List
import PokerData

data PokerRank = HighCard Card
               | Pair (Rank, Card)
               | TwoPair Hand
               | ThreeOfAKind Rank
               | Straight Card
               | Flush Suit
               | FullHouse Rank
               | FourOfAKind Rank
               | StraitFlush Card
               deriving (Eq, Ord, Show)

findRank :: Hand -> PokerRank
findRank cards
  | isStraightFlush cards = StraitFlush  $ maximum cards    
  | isFourOfAKind cards   = FourOfAKind  $ extract cards    
  | isFullHouse cards     = FullHouse    $ extract cards    
  | isFlush cards         = Flush        $ snd $ head cards 
  | isStraight cards      = Straight     $ maximum cards    
  | isThreeOfAKind cards  = ThreeOfAKind $ extract cards
  | isTwoPair cards       = TwoPair      $ sort cards
  -- TODO this is no good. must get the maximum excluding the pair.
  | isPair cards          = Pair         (extract cards, maximum cards)  
  | otherwise             = HighCard     $ maximum cards       
  where extract = fst . snd . maximum . zipCountCollected . collectBy fst


-- Rank Predicate Tests
isPair :: Hand -> Bool
isPair = (2 `elem`) . countCollectedRank 

isTwoPair :: Hand -> Bool
isTwoPair cards = length counted == 2
  where pairCount = countCollectedRank cards
        counted = elemIndices 2 pairCount

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind  = (3 `elem`) . countCollectedRank 

isStraight :: Hand -> Bool
isStraight cards =  range == range'
  where range = sort $ getRanks cards
        range' = [head range .. last range]

isFlush :: Hand -> Bool
isFlush  = (5 `elem`) . countCollectedSuite

isFullHouse :: Hand -> Bool
isFullHouse cards = isThreeOfAKind cards && isPair cards

isFourOfAKind :: Hand -> Bool
isFourOfAKind = (4 `elem`) . countCollectedRank

isStraightFlush :: Hand -> Bool
isStraightFlush cards = isFlush cards && isStraight cards


-- Utils  
collectBy :: Ord b => (t -> b) -> [t] -> [[t]]
collectBy fn = groupBy comp . sortOn fn
  where comp c1 c2 = fn c1 == fn c2

zipCountCollected :: [[t]] -> [(Int, t)]
zipCountCollected = map simpler
  where simpler xs = (length xs, head xs)

countCollected :: Foldable t => (a -> [t a1]) -> a -> [Int]
countCollected collector = (map length) . collector

countCollectedRank :: Deck -> [Int]
countCollectedRank = countCollected $ collectBy fst

countCollectedSuite :: Deck -> [Int]
countCollectedSuite = countCollected $ collectBy snd

getRanks :: Hand -> [Rank]
getRanks = map fst

getSuits :: Hand -> [Suit]
getSuits = map snd