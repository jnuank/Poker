module Poker.Hand where

import Data.List
import Control.Applicative
import Data.Function

import Card

-- 役のデータ構造
data Hand = HighCard | Flush | Straight | Pair | StraightFlush | ThreeCard
          | RoyalStraightFlush | FourCard | FullHouse | TwoPair
  deriving (Show, Ord, Eq)

-- 役の判定で使用する関数群
isPair :: [Card] -> Bool
isPair cards = or [ x `hasSameRank` y | x <- cards, y <- cards, x /= y]

isTwoPair :: [Card] -> Bool
isTwoPair = (== 2) . count (== 2) . rankCounts
  where
    count ::(a -> Bool) -> [a] -> Int
    count f = length . filter f

isFlush :: [Card] -> Bool
isFlush (card:cards) = all (hasSameSuit card) cards

isThreeCard :: [Card] -> Bool
isThreeCard = elem 3 . rankCounts

isFourCard :: [Card] -> Bool
isFourCard = elem 4 . rankCounts

isFullHouse :: [Card] -> Bool
isFullHouse = has2And3 . rankCounts
  where
    has2And3 :: [Int] -> Bool
    has2And3 = liftA2 (&&) (elem 2) (elem 3)

rankCounts :: [Card] -> [Int]
rankCounts = map length . groupBy (==) . sort . map (fromEnum . rank)

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

