{-# LANGUAGE TypeFamilyDependencies #-}
module Poker.FiveCards where

import Data.Function
import Data.Ord
import Data.List

import Card
import Poker.Hand
import Poker.Cards
import Poker.Ord

data FiveCards = FiveCards Card Card Card Card Card
  deriving (Show, Eq)

newtype FiveCardHand = FiveCardHand { fromFiveCardHand :: Hand } deriving (Eq)
instance Ord FiveCardHand where 
  compare = compare `on` strength . fromFiveCardHand
    where
      hands = [HighCard, Pair, TwoPair, ThreeCard, Straight, Flush, FullHouse, FourCard, StraightFlush, RoyalStraightFlush]
      strength :: Hand -> Int
      strength hand = case elemIndex hand hands of (Just i) -> i
      
instance Cards FiveCards where
    type HandOf FiveCards = FiveCardHand 
    hand (FiveCards first second third fourth fifth)
      | isRoyalStraightFlush cards = RoyalStraightFlush
      where 
        cards = [first, second, third, fourth, fifth]
    wrapHand = undefined
    orderCards = undefined
  
isRoyalStraightFlush :: [Card] -> Bool
isRoyalStraightFlush cards = isFlush cards && isRoyalRank
  where
    sorted = sort cards
    isRoyalRank =
      case sorted of
        [Card _ Ten, Card _ Jack, Card _ Queen, Card _ King, Card _ Ace] -> True
        _ -> False
