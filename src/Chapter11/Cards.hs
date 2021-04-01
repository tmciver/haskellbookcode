module Chapter11.Cards where

data Suit =
    Hearts
  | Diamonds
  | Clubs
  | Spades
  deriving (Eq, Show, Enum)

data Value =
    Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Show, Enum)

data Card = Card Suit Value
  deriving (Eq, Show)

--data Deck = Deck [Card]
type Deck = [Card]

sortedDeck :: Deck
sortedDeck = [Card suit val | suit <- enumFrom Hearts, val <- enumFrom Two]
