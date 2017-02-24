module PokerRank ( findRank ) where
import Data.List
import Hands

data PokerRank = HighCard Card
               | Pair Rank
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
  | isPair cards          = Pair         $ extract cards    
  | otherwise             = HighCard $ maximum cards       
  where extract = fst . snd . maximum . countCollected . collectByRank



-- Rank Predicate Tests

isPair :: Hand -> Bool
isPair cards = 2 `elem` map length (collectByRank cards)

isTwoPair :: Hand -> Bool
isTwoPair cards = length counted == 2
  where pairCount = map length $ collectByRank cards
        counted = elemIndices 2 pairCount

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind cards = 3 `elem` map length (collectByRank cards)

isStraight :: Hand -> Bool
isStraight cards =  range == range'
  where range = sort $ getRanks cards
        range' = [head range .. last range]

isFlush :: Hand -> Bool
isFlush cards = 5 `elem` map length (collectBySuit cards)

isFullHouse :: Hand -> Bool
isFullHouse cards = isThreeOfAKind cards && isPair cards

isFourOfAKind :: Hand -> Bool
isFourOfAKind cards = 4 `elem` map length (collectByRank cards)

isStraightFlush :: Hand -> Bool
isStraightFlush cards = isFlush cards && isStraight cards


-- Utils
collectBySuit :: Deck -> [Deck]
collectBySuit = collectBy snd
  
collectByRank :: Deck -> [Deck]
collectByRank = collectBy fst

collectBy :: Ord b => (t -> b) -> [t] -> [[t]]
collectBy fn = groupBy comp . sortOn fn
  where comp c1 c2 = fn c1 == fn c2

countCollected :: [[t]] -> [(Int, t)]
countCollected xss = map simpler xss
  where simpler xs = (length xs, head xs)

getRanks :: Hand -> [Rank]
getRanks = map fst

getSuits :: Hand -> [Suit]
getSuits = map snd
