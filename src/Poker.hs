module Poker where

data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Eq)

data Suit = Spade | Heart | Club | Diamond
  deriving (Eq)

data Rank
  = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King
  deriving (Ord, Eq, Enum)

instance Show Suit where
  show Spade = "♠"
  show Heart = "♥"
  show Club = "♣"
  show Diamond = "♦︎"

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

data Hand = Pair | Flush | HighCard | Straight | StraightFlush
  deriving (Show, Eq)

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
