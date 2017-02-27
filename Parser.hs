{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( handParser
  , validatehand
  )
where

import Text.ParserCombinators.Parsec (ParseError, Parser, parse, spaces, oneOf, (<|>), sepEndBy)
import Data.List 
import Data.Char (toUpper, toLower)
import Text.ParserCombinators.Parsec.Error
import Data.Maybe (fromJust)
import PokerData 


-- this order needs to match the order the data types are defined instance
ranks = "234567890JQKA"
suits = "SHDC"

handParser :: String -> Either ParseError [Card]
handParser = parse parseHand "" 

parseHand :: Parser Hand
parseHand =  spaces *> parseCard `sepEndBy` spaces

parseCard :: Parser Card
parseCard = (,) <$> parseRank <*> parseSuit
  where parseRank = parseZip ranks [Two ..]
        parseSuit = parseZip suits [Spade ..]
   
parseZip :: String -> [b] -> Parser b
parseZip xs ys = fromJust . flip lookup (zip xs ys) <$> oneOf' xs  


oneOf' xs = toUpper <$> (oneOf xs''<|> oneOf xs')
  where xs'  = map toLower xs
        xs'' = map toUpper xs


validatehand parsed =
  case parsed of
    Right h -> do
      if isValid h
        then Right h
        else Left  "Invalid Hand"
    Left er -> Left $ "hmm"
  where isValid h = length (nub h) == 5
