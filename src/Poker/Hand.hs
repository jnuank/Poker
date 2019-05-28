module Poker.Hand where

import Data.List

import Card

-- 役のデータ構造
data Hand = HighCard | Flush | Straight | Pair | StraightFlush | ThreeCard 
          | RoyalStraightFlush | FourCard | FullHouse | TwoPair
  deriving (Show, Ord, Eq)

-- 役の判定で使用する関数群
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

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards
