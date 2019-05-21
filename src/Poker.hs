module Poker where

import Data.List
import Card
import Data.Function
import Data.Ord

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

hand :: Cards -> Hand
hand (Cards first second)
  | isStraightFlush cards = StraightFlush
  | isPair cards = Pair
  | isStraight cards = Straight
  | isFlush cards = Flush
  | otherwise = HighCard
  where
    cards = [first, second]  

threeHand :: ThreeCards -> Hand 
threeHand (ThreeCards first second third) 
  | isStraightFlush cards = StraightFlush
  | isThreeCard cards = ThreeCard
  | isStraight cards = Straight
  | isFlush cards = Flush
  | isPair cards = Pair
  | otherwise = HighCard
  where 
    cards = [first,second,third]

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

data Cards = Cards Card Card
  deriving (Show, Eq)

data ThreeCards = ThreeCards Card Card Card
  deriving (Show, Eq)

instance Ord Cards where
  compare left right = case (compare `on` TwoCardHand . hand) left right of
    EQ -> compareCardsWithRank left right
    other -> other

instance Ord ThreeCards where 
  compare = compareWithTpl (ThreeCardHand . threeHand, orderCards)
    where 
      orderCards :: ThreeCards -> [Card]
      orderCards (ThreeCards first second third) = orderingCardsWithHand [first, second, third]
  --  compare = compare `on` ThreeCardHand . threeHand

compareWithTpl :: (Ord a, Ord b) => (c -> a, c -> b) -> c -> c -> Ordering 
compareWithTpl (f, g) = compare `on` do 
  a <- f
  b <- g
  return (a, b)

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

lowestRank :: [Rank] -> Rank 
lowestRank [Ace, Two] = Ace
lowestRank [Ace, Three, Two] = Ace
lowestRank ranks = minimum ranks

-- 役によって、カード自体を並び替えして返す
orderingCardsWithHand :: [Card] -> [Card]
orderingCardsWithHand cards = 
  case sorted of 
    [f@(Card _ Ace), s@(Card _ Three), t@(Card _ Two)] -> [s, t, f]
    [f, s, t] | rank s == rank t -> [s, t, f]
    other -> other         
    where
      sorted = sortOn Down cards