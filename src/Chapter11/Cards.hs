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

-- What's the cardinality of Card?
--
-- cardinality of suit * cardinality of Value + 1 (Joker1) + 1 (Joker2)
-- 4 * 13 + 2
-- 54
data Card = Card Suit Value
          | Joker1
          | Joker2
  deriving (Eq, Show)

showCard :: Card -> String
showCard (Card suit value) = show value ++ " of " ++ show suit
showCard Joker1 = "First Joker"
showCard Joker2 = "Second Joker"

--data Deck = Deck [Card]
type Deck = [Card]

sortedDeck :: Deck
sortedDeck = Joker1 : Joker2 : [Card suit val | suit <- enumFrom Hearts, val <- enumFrom Two]
