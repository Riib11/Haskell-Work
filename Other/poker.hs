import System.Random
-- import Control.Monad.Random

(#) :: Suit -> Rank -> Card
s # r = Card r s

data Card = Card{ rank::Rank, suit::Suit }
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving(Ord,Bounded,Enum,Eq,Show,Read)
data Rank = Ace | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving(Ord,Bounded,Enum,Eq,Show,Read)
instance Show Card where
    show (Card rank suit) = show rank ++ " Of " ++ show suit

deck = (#) <$> [Clubs .. Spades] <*> [Ace .. King]

type Seed = Int
randomStream :: Seed -> [Int]
random s = mersenneTwiserPerturb s : randomStream $ splitSeed s

shuffleHelper :: [Int] -> [a] -> [a]
shuffleHelper (i:is) xs =   let (first,rest) = splitAt (mod i $ length xs) xs
                            in (last,firsts) : shuffle is (init firsts ++ rest)

shuffle :: Seed -> [a] -> [a]
shuffle s xs = shuffleHelper (randomStream s) xs