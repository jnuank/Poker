module Poker where

data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Eq)

data Suit = Spade | Heart | Club | Diamond
  deriving (Eq)

instance Show Suit where
  show Spade = "♠"
  show Heart = "♥"
  show Club = "♣"
  show Diamond = "♦︎"
  
data Rank
  = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King
  deriving (Eq, Enum)
-- newtype PokerRank = PokerRank Rank
-- instance Ord PokerRank where
--   compare (PokerRank left) (_ right) = ...

-- Aが一番上。
instance Ord Rank where
  _ <= Ace = True
  Ace <= _ = False
  left <= right = fromEnum left <= fromEnum right

instance Show Rank where
  show Ace = "A"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show rank = show num
    where num = fromEnum rank + 1

instance Show Card where
  show (Card suit rank) = show rank ++ show suit

hasSameSuit :: Card -> Card -> Bool
hasSameSuit (Card suit _) (Card suit' _) = suit == suit'

hasSameRank :: Card -> Card -> Bool 
hasSameRank (Card _ rank) (Card _ rank') = rank == rank'

data Cards = Cards Card Card
  deriving (Show, Eq)

instance Ord Cards where
  compare left right = case compare (hand left) (hand right) of
    EQ -> compareCards left right
    other -> other
  --left <= right = hand left <= hand right
  
data Hand =  HighCard | Flush | Straight | Pair | StraightFlush
  deriving (Show, Ord, Eq)

compareCards :: Cards -> Cards -> Ordering
compareCards left right = case hand left of 
  Flush -> if ordOfMax == EQ then ordOfMin else ordOfMax
  HighCard -> if ordOfMax == EQ then ordOfMin else ordOfMax
  -- Flush -> ordOfMax
  -- HighCard -> ordOfMax
  other -> ordOfMax
  where
    (firstMax, firstMin) = sortingCard left
    (secondMax, secondMin) = sortingCard right
    ordOfMax = rank firstMax `compare` rank secondMax
    ordOfMin = rank firstMin `compare` rank secondMin

sortingCard :: Cards -> (Card, Card)
sortingCard (Cards first second)
  | (rank first) == Ace && (rank second) == Two  = (second, first)
  | (rank first) == Two && (rank second) == Ace  = (first, second)
  | otherwise = sortingBy rank first second

sortingBy :: Ord a => (b -> a) -> b -> b -> (b, b)
sortingBy f left right = case f left > f right of
  True -> (left, right)
  _ -> (right, left)
  
hand :: Cards -> Hand
hand (Cards first second)
  | isStraight && isFlush = StraightFlush
  | isPair = Pair
  | isStraight = Straight
  | isFlush = Flush
  | otherwise = HighCard
  where
    isPair = first `hasSameRank` second
    isFlush = first `hasSameSuit` second
    isStraight = rank first `isConsecutiveRank` rank second
  
isConsecutiveRank :: Rank -> Rank -> Bool
isConsecutiveRank King Ace = True
isConsecutiveRank Ace King = True
isConsecutiveRank first second = abs (fromEnum first - fromEnum second) == 1
isConsecutiveRank _ _ = False
