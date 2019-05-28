{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Poker.Cards where

import Card
import Poker.Hand

-- TwoCard、ThreeCardを共通化する為の型クラス
class Cards a where
  type HandOf a = (h :: *) | h -> a
  hand :: a -> Hand
  wrapHand :: Ord (HandOf a) => Hand -> HandOf a
  orderCards :: a -> [Card]

