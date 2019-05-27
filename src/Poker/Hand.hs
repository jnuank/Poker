{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Poker.Hand where 

import Data.List
import Card
import Data.Function
import Data.Ord

data Hand =  HighCard | Flush | Straight | Pair | StraightFlush | ThreeCard
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
  

class Cards a where 
  type HandOf a = (h :: *) | h -> a
  hand :: a -> Hand
  fromCards :: a -> [Card]
  wrapHand :: Ord (HandOf a) => Hand -> HandOf a

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
  fromCards (TwoCards first second) = [first, second]
  wrapHand = TwoCardHand

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
  fromCards (ThreeCards first second third) = [first, second, third]
  wrapHand = ThreeCardHand

data TwoCards = TwoCards Card Card
  deriving (Show, Eq)

data ThreeCards = ThreeCards Card Card Card
  deriving (Show, Eq)

isPair :: [Card] -> Bool
isPair cards = or [ x `hasSameRank` y | x <- cards, y <- cards, x /= y]

isFlush :: [Card] -> Bool
isFlush (card:cards) = all (hasSameSuit card) cards

isThreeCard :: [Card] -> Bool
isThreeCard  (card:cards) = all (hasSameRank card) cards

isStraight :: [Card] -> Bool
isStraight = isConsecutiveRanks . map rank
  where
    isConsecutiveRanks :: [Rank] -> Bool
    isConsecutiveRanks = or . map (scanPairs (\l r -> r - l == 1)) . orderCandidates

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards


orderCandidates :: [Rank] -> [[Int]]
orderCandidates ranks = map sort $ sequence $ map candidates ranks
  where
    candidates :: Rank -> [Int]
    candidates Ace = [1, 14]
    candidates x = [fromEnum x + 1]

scanPairs :: (a -> a -> Bool) -> [a] -> Bool
scanPairs f [left, right] = f left right
scanPairs f (left:right:rest)
  | f left right = scanPairs f (right:rest)
  | otherwise = False

