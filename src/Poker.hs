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
  | isStraight && isFlush = StraightFlush
  | isPair [first, second] = Pair
  | isStraight = Straight
  | isFlush = Flush
  | otherwise = HighCard
  where
    isFlush = first `hasSameSuit` second
    isStraight = rank first `isConsecutiveRank` rank second

threeHand :: ThreeCards -> Hand 
threeHand (ThreeCards first second third) 
  | isThreeCard = ThreeCard
  | isStraight = Straight
  | isFlush = Flush
  | isPair [first,second,third] = Pair
  | otherwise = HighCard
  where 
    isThreeCard = all (hasSameRank first) [second,third]
    isFlush = all (hasSameSuit first) [second,third]
    isStraight = isConsecutiveThreeRank (rank first) (rank second) (rank third)

isPair :: [Card] -> Bool
isPair cards = or [ x `hasSameRank` y | x <- cards, y <- cards, x /= y]

isConsecutiveRank :: Rank -> Rank -> Bool
isConsecutiveRank King Ace = True
isConsecutiveRank Ace King = True
isConsecutiveRank first second = abs (fromEnum first - fromEnum second) == 1

isConsecutiveThreeRank :: Rank -> Rank -> Rank -> Bool 
isConsecutiveThreeRank first second third = 
  let sorted = map sort $ order first second third
  in or $ map (hoge (\l r -> r - l == 1)) sorted  

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

orderCandidates :: Rank -> [Int]
orderCandidates Ace = [1, 14]
orderCandidates x = [fromEnum x + 1]

isStraight :: [Int] -> Bool
isStraight ranks = last sorted - head sorted == 2
  where 
    sorted = sort ranks

order f s t = do
  f' <- orderCandidates f
  s' <- orderCandidates s 
  t' <- orderCandidates t
  return [f', s', t']

hoge :: (a -> a -> Bool) -> [a] -> Bool
hoge f [left, right] = f left right
hoge f (left:right:rest)
  | f left right = hoge f (right:rest)
  | otherwise = False
