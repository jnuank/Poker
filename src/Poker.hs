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

hand :: Cards -> Hand
hand (Cards first second)
  | isStraight cards && isFlush = StraightFlush
  | isPair cards = Pair
  | isStraight cards = Straight
  | isFlush = Flush
  | otherwise = HighCard
  where
    cards = [first, second]  
    isFlush = first `hasSameSuit` second

threeHand :: ThreeCards -> Hand 
threeHand (ThreeCards first second third) 
  | isThreeCard = ThreeCard
  | isStraight cards = Straight
  | isFlush = Flush
  | isPair cards = Pair
  | otherwise = HighCard
  where 
    cards = [first,second,third]
    isThreeCard = all (hasSameRank first) [second,third]
    isFlush = all (hasSameSuit first) [second,third]

isPair :: [Card] -> Bool
isPair cards = or [ x `hasSameRank` y | x <- cards, y <- cards, x /= y]

isStraight :: [Card] -> Bool
isStraight = isConsecutiveRanks . map rank
  where 
    isConsecutiveRanks :: [Rank] -> Bool 
    isConsecutiveRanks = or . map (scanPairs (\l r -> r - l == 1)) . orderCandidates  

data Cards = Cards Card Card
  deriving (Show, Eq)

data ThreeCards = ThreeCards Card Card Card
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
