module Poker where

import Data.List
import Card

instance Ord Rank where
  _ <= Ace = True
  Ace <= _ = False
  left <= right = fromEnum left <= fromEnum right

instance Ord Card where
  compare left right = rank left `compare` rank right

data Hand =  HighCard | Flush | Straight | Pair | StraightFlush
  deriving (Show, Ord, Eq)

hand :: Cards -> Hand
hand (Cards first second)
  | isStraight && isFlush = StraightFlush
  | isPair = Pair
  | isStraight = Straight
  | isFlush = Flush
  | otherwise = HighCard
  where
    isPair = first `hasSameRank` second
    isFlush = first `hasSameSuit` second
    isStraight = rank first `isConsecutiveRank` rank second
  
isConsecutiveRank :: Rank -> Rank -> Bool
isConsecutiveRank King Ace = True
isConsecutiveRank Ace King = True
isConsecutiveRank first second = abs (fromEnum first - fromEnum second) == 1

data Cards = Cards Card Card
  deriving (Show, Eq)

instance Ord Cards where
  compare left right = case compare (hand left) (hand right) of
    EQ -> compareCardsWithRank left right
    other -> other

compareCardsWithRank :: Cards -> Cards -> Ordering
compareCardsWithRank left right = sortedLeft `compare` sortedRight
  where
    sortedLeft = sortCards left
    sortedRight = sortCards right
    sortCards :: Cards -> [Card]
    sortCards (Cards left right) = case descSortedCards of
      [ace@(Card _ Ace), two@(Card _ Two)] -> [two, ace]
      other -> other
      where descSortedCards = reverse . sort $ [left, right]
