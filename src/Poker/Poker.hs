module Poker.Poker where

import Data.List
import Card
import Data.Function
import Data.Ord
import Poker.Hand

instance Ord Rank where
  _ <= Ace = True
  Ace <= _ = False
  left <= right = fromEnum left <= fromEnum right

-- カードの比較はランクで比較する
instance Ord Card where
  compare = compare `on` rank

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

instance Ord TwoCards where
  compare = compareWithTpl (TwoCardHand . hand, orderCards)
    where 
      orderCards :: TwoCards -> [Card]
      orderCards = orderingCardsWithHand . fromCards 

instance Ord ThreeCards where
  compare = compareWithTpl (ThreeCardHand . hand, orderCards)
    where
      orderCards :: ThreeCards -> [Card]
      orderCards = orderingCardsWithHand . fromCards

compareWithTpl :: (Ord a, Ord b) => (c -> a, c -> b) -> c -> c -> Ordering
compareWithTpl (f, g) = compare `on` do
  a <- f
  b <- g
  return (a, b)
lowestRank :: [Rank] -> Rank
lowestRank [Ace, Two] = Ace
lowestRank [Ace, Three, Two] = Ace
lowestRank ranks = minimum ranks

-- 役によって、カード自体を並び替えして返す
orderingCardsWithHand :: [Card] -> [Card]
orderingCardsWithHand cards =
  case sorted of
    [f@(Card _ Ace), s@(Card _ Two)] -> [s, f]
    [f@(Card _ Ace), s@(Card _ Three), t@(Card _ Two)] -> [s, t, f]
    [f, s, t] | rank s == rank t -> [s, t, f]
    other -> other
    where
      sorted = sortOn Down cards