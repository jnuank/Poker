{-# LANGUAGE FlexibleContexts #-}

module Poker.Ord where

import Data.Ord
import Data.List
import Data.Function

import Card
import Poker.Hand
import Poker.Cards

instance Ord Rank where
  _ <= Ace = True
  Ace <= _ = False
  left <= right = fromEnum left <= fromEnum right

instance Ord Card where
  compare = compare `on` rank

compare' :: (Cards a, Ord (HandOf a)) => a -> a -> Ordering
compare' = compareWithTpl (hand', orderCards)
  where
    hand' :: (Cards a, Ord (HandOf a)) => a -> HandOf a
    hand' = wrapHand . hand
    compareWithTpl :: (Ord a, Ord b) => (c -> a, c -> b) -> c -> c -> Ordering
    compareWithTpl (f, g) = compare `on` do
      a <- f
      b <- g
      return (a, b)