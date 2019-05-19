module Poker where

import Data.List
import Card
import Data.Function

instance Ord Rank where
  _ <= Ace = True
  Ace <= _ = False
  left <= right = fromEnum left <= fromEnum right

-- カードの比較はランクで比較する
instance Ord Card where
  compare = compare `on` rank

data Hand =  HighCard | Flush | Straight | Pair | StraightFlush
  deriving (Show, Ord, Eq)

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
  compare left right = case (compare `on` TwoCardHand . hand) left right of
    EQ -> compareCardsWithRank left right
    other -> other

compareCardsWithRank :: Cards -> Cards -> Ordering
compareCardsWithRank = compare `on` sortCards
  where
    sortCards :: Cards -> [Card]
    sortCards (Cards left right) = case descSortedCards of
      [ace@(Card _ Ace), two@(Card _ Two)] -> [two, ace]
      other -> other
      where descSortedCards = reverse . sort $ [left, right]
