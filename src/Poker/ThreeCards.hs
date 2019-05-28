{-# LANGUAGE TypeFamilyDependencies #-}
module Poker.ThreeCards where

import Data.Function
import Data.Ord
import Data.List

import Card
import Poker.Hand
import Poker.Cards
import Poker.Ord

data ThreeCards = ThreeCards Card Card Card
  deriving (Show, Eq)

newtype ThreeCardHand = ThreeCardHand { fromThreeCardHand :: Hand } deriving (Eq)
instance Ord ThreeCardHand where
  compare = compare `on` strength . fromThreeCardHand
    where
      strength :: Hand -> Int
      strength StraightFlush = 6
      strength ThreeCard = 5
      strength Straight = 4
      strength Flush = 3
      strength Pair = 2
      strength HighCard = 1

instance Cards ThreeCards where 
  type HandOf ThreeCards = ThreeCardHand
  hand (ThreeCards first second third)
    | isStraightFlush cards = StraightFlush
    | isThreeCard cards = ThreeCard
    | isStraight cards = Straight
    | isFlush cards = Flush
    | isPair cards = Pair
    | otherwise = HighCard
    where
      cards = [first,second,third]
  wrapHand = ThreeCardHand
  orderCards (ThreeCards first second third) =
      case sorted of 
        [f@(Card _ Ace), s@(Card _ Three), t@(Card _ Two)] -> [s, t, f]
        [f, s, t] | rank s == rank t -> [s, t, f]
        other -> other
    where
      sorted = sortOn Down [first, second, third]

instance Ord ThreeCards where
  compare = compare'
