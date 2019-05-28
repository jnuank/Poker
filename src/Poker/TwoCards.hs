{-# LANGUAGE TypeFamilyDependencies #-}
module Poker.TwoCards where 
  
import Data.Function
import Data.Ord
import Data.List

import Card
import Poker.Hand
import Poker.Cards
import Poker.Ord
  
data TwoCards = TwoCards Card Card
  deriving (Show, Eq)

newtype TwoCardHand = TwoCardHand { fromTwoCardHand :: Hand } deriving (Eq)
instance Ord TwoCardHand where
  compare  = compare `on` strength . fromTwoCardHand
    where
      strength :: Hand -> Int
      strength StraightFlush = 5
      strength Pair = 4
      strength Straight = 3
      strength Flush = 2
      strength HighCard = 1

instance Cards TwoCards where 
  type HandOf TwoCards = TwoCardHand
  hand (TwoCards first second)
    | isStraightFlush cards = StraightFlush
    | isPair cards = Pair
    | isStraight cards = Straight
    | isFlush cards = Flush
    | otherwise = HighCard
    where
      cards = [first, second]
  wrapHand = TwoCardHand
  orderCards (TwoCards first second) = 
      case sorted of
        [f@(Card _ Ace), s@(Card _ Two)] -> [s, f]
        other -> other
    where
      sorted = sortOn Down [first, second]

instance Ord TwoCards where
  compare = compare'
  