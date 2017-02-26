{-# LANGUAGE FlexibleContexts #-}
module Parser
  (handParser)
where

import Text.ParserCombinators.Parsec (ParseError, Parser, parse, spaces, oneOf, (<|>), sepEndBy)
import Data.Char (toUpper, toLower)
import Data.Maybe (fromJust)
import PokerData 


-- this order needs to match the order the data types are defined instance
ranks = "23456789TJQKA"
suits = "SHDC"

handParser :: String -> Either ParseError [Card]
handParser = parse parseHand "" 

parseHand :: Parser Hand
parseHand =  parseCard `sepEndBy` spaces

parseCard :: Parser Card
parseCard = (,) <$> parseRank <*> parseSuit
  where parseRank = parseZip ranks [Two ..]
        parseSuit = parseZip suits [Spade ..]
   
parseZip :: String -> [b] -> Parser b
parseZip xs ys = fromJust . flip lookup (zip xs ys) <$> oneOf' xs  


oneOf' xs = toUpper <$> (oneOf xs''<|> oneOf xs')
  where xs'  = map toLower xs
        xs'' = map toUpper xs
