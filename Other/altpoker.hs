-- example: ace Of Spades:
ace :: a -> Suit -> Card; ace _ s = Card Ace s
one :: a -> Suit -> Card; one _ s = Card One s
two :: a -> Suit -> Card; two _ s = Card Two s
three :: a -> Suit -> Card; three _ s = Card Three s
four :: a -> Suit -> Card; four _ s = Card Four s
five :: a -> Suit -> Card; five _ s = Card Five s
six :: a -> Suit -> Card; six _ s = Card Six s
seven :: a -> Suit -> Card; seven _ s = Card Seven s
eight :: a -> Suit -> Card; eight _ s = Card Eight s
nine :: a -> Suit -> Card; nine _ s = Card Nine s
ten :: a -> Suit -> Card; ten _ s = Card Ten s
jack :: a -> Suit -> Card; jack _ s = Card Jack s
queen :: a -> Suit -> Card; queen _ s = Card Queen s
king :: a -> Suit -> Card; king _ s = Card King s

data Of = Of
data Card = Card{ rank::Rank, suit::Suit }
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving(Ord,Bounded,Enum,Eq,Show,Read)
data Rank = Ace | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving(Ord,Bounded,Enum,Eq,Show,Read)
instance Show Card where
    show (Card rank suit) = show rank ++ " Of " ++ show suit