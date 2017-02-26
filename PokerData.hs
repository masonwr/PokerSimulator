{-# LANGUAGE UnicodeSyntax #-}

module PokerData where

type HandStr = String

data Suit = Spade
          | Heart
          | Dimond
          | Club
          deriving (Eq, Ord, Enum)

instance Show Suit where
  show Spade = "♠"
  show Heart = "❤"
  show Club = "♣"
  show Dimond = "♦"

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Ord, Enum, Show)


type Card = (Rank, Suit)
type Deck = [Card]
type Hand = Deck


standardDeck :: Deck
standardDeck  = [(r,s) | s <- [Spade ..], r <- [Two ..]]




-- straitFlush = "AH JH QH KH 0H"
