{-# LANGUAGE UnicodeSyntax #-}

module Hands where

type HandStr = String

data Suit = Spade
          | Heart
          | Dimond
          | Clubs
          deriving (Eq, Ord, Enum)

instance Show Suit where
  show Spade = "♠"
  show Heart = "❤"
  show Clubs = "♣"
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


deck :: Deck
deck  = [(r,s) | s <- [Spade ..], r <- [Two ..]]


randoHAnd :: Hand
randoHAnd = take 5 deck



straitFlush = "AH JH QH KH 0H"
