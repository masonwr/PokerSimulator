{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser
  (parseInput)
where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Applicative hiding ((<|>))
import PokerData 

oneOf' xs = toUpper <$> ((oneOf xs'') <|> (oneOf xs'))
  where xs'  = map toLower xs
        xs'' = map toUpper xs

-- this order needs to match the order the data types are defined in.
ranks = "23456789TJQKA"
suits = "SHDC"


parseInput :: String -> Either ParseError [Card]
parseInput str = parse parseHand "" str 

parseHand :: Parser Hand
parseHand =  parseCard `sepEndBy` spaces

parseCard :: Parser Card
parseCard = (,) <$> parseRank <*> parseSuit
  
parseRank :: Parser Rank
parseRank = parseZip ranks [Two ..]
   
parseSuit :: Parser Suit
parseSuit = parseZip suits [Spade ..]

parseZip :: String -> [b] -> Parser b
parseZip xs ys = fromJust . flip lookup (zip xs ys) <$> oneOf' xs  

  

     
       


  
  
