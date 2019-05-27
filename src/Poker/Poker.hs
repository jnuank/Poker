{-# LANGUAGE FlexibleContexts #-}

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

compare' :: (Cards a, Ord (HandOf a)) => a -> a -> Ordering
compare' = compareWithTpl (hand', orderCards)
  where
    hand' :: (Cards a, Ord (HandOf a)) => a -> HandOf a
    hand' = wrapHand . hand
    orderCards = orderingCardsWithHand . fromCards

instance Ord TwoCards where
  compare = compare'

instance Ord ThreeCards where
  compare = compare'

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