module PokerRank where
import Data.List
import Hands

data PokerRank = HighCard Card
               | Pair Rank
               | TwoPair (Rank, Rank)
               | ThreeOfAKind Rank
               | Straight Card
               | Flush Suit
               | FullHouse (Rank, Rank)
               | FourOfAKind Rank
               | StraitFlush Card
               deriving (Eq, Ord, Show)

findRank :: Hand -> PokerRank
findRank cards
  | isPair cards          = HighCard $ maximum cards
  | isStraightFlush cards = StraitFlush $ maximum cards
  | otherwise             = HighCard $ maximum cards


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
collectBySuit = groupBy sameSuit . sortOn snd

collectByRank :: Deck -> [Deck]
collectByRank = groupBy sameRank . sortOn fst

sameSuit :: Card -> Card -> Bool
sameSuit (_, r1) (_, r2) = r1 == r2

sameRank :: Card -> Card -> Bool
sameRank (s1, _) (s2, _) = s1 == s2


getRanks :: Hand -> [Rank]
getRanks = map fst

getSuits :: Hand -> [Suit]
getSuits = map snd
