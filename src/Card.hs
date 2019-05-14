module Card where 

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
