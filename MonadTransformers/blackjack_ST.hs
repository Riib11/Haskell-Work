{-# LANGUAGE TemplateHaskell #-}

import System.Random
import Control.Monad
import Control.Lens
import Data.Char (toLower)

-----------------------------------------------------------------------------------------------------------------------------
--  Data Type
-----------------------------------------------------------------------------------------------------------------------------

--
-- Card
--

data Card = Card { suite :: Suite, rank :: Rank }

instance Show Card where show c = show (suite c) ++ ":" ++ show (rank c)

data Suite = S_Spades | S_Clubs | S_Diamonds | S_Hearts
  deriving (Eq, Enum)

instance Show Suite where
  show S_Spades   = "S"
  show S_Clubs    = "C"
  show S_Diamonds = "D"
  show S_Hearts   = "H"

data Rank = R_Ace | R_2 | R_3 | R_4 | R_5 | R_6 | R_7 | R_8 | R_9 | R_10 | R_Jack | R_Queen | R_King
  deriving (Eq, Enum)

instance Show Rank where
  show R_Ace   = "A"
  show R_Jack  = "J"
  show R_Queen = "Q"
  show R_King  = "K"
  show rank    = show . (+1) . fromEnum $ rank

get_rank_value :: Rank -> Int
get_rank_value r = case r of
  R_Jack  -> 10
  R_Queen -> 10
  R_King  -> 10
  _       -> fromEnum r + 1

--
-- Card-stack
--

type Hand = [Card]
type Deck = [Card]

deck_init = [ Card s r | s <- [S_Spades .. S_Hearts], r <- [R_Ace .. R_King] ]

--
-- BJState
--

data BJState = BJState
  { _player :: Hand
  , _dealer :: Hand
  , _deck   :: Deck }

$(makeLenses ''BJState)

init_state = BJState [] [] []

-----------------------------------------------------------------------------------------------------------------------------
--  Score
-----------------------------------------------------------------------------------------------------------------------------

data Score = Score { low_score :: Int, high_score :: Int }
  deriving (Show)

get_card_score :: Card -> Score
get_card_score c = case rank c of
  R_Ace   -> Score 1  11
  r       -> Score v  v where v = get_rank_value r

sum_scores :: [Score] -> Score
sum_scores ss = Score (total low_score) (total high_score)
  where total get_card_score = sum $ map get_card_score ss

get_hand_total :: Hand -> Int
get_hand_total hand = if high_score score <= 21 then high_score score else low_score score
  where score = sum_scores $ map get_card_score hand

get_hand_totals :: BJState -> (Int, Int)
get_hand_totals s = (p,d) where [p,d] = map (\t -> get_hand_total $ view t s) [player, dealer]

-----------------------------------------------------------------------------------------------------------------------------
--  Show
-----------------------------------------------------------------------------------------------------------------------------

splice_strings :: String -> [String] -> String
splice_strings sep ss = case ss of { [] -> "" ; s:[] -> s ; s:ss' -> s ++ sep ++ splice_strings sep ss' }

show_hand :: (Card -> String) -> Hand -> String
show_hand show_card cs = splice_strings ", " $ map show_card cs

show_revealed_hand :: Hand -> String
show_revealed_hand = show_hand show

show_peeking_hand :: Hand -> String
show_peeking_hand (c:cs) = show c ++ ", " ++ show_hand show_hidden_card cs

show_hidden_card :: Card -> String
show_hidden_card _ = "###"

show_state :: Bool -> BJState -> String
show_state is_midgame s =
  "player: " ++ (show_revealed_hand $ view player s) ++ "\n" ++
  "dealer: " ++ (if is_midgame then show_peeking_hand (view dealer s) else show_revealed_hand (view dealer s)) ++ "\n" ++
  (take 40 $ repeat '-')

display_state :: Bool -> BJState -> IO ()
display_state hidden s = putStrLn $ show_state hidden s

-----------------------------------------------------------------------------------------------------------------------------
--  Shuffle
-----------------------------------------------------------------------------------------------------------------------------

--
-- GenUpdate
--

type Gen = StdGen

newtype GenUpdate a = GenUpdate (Gen -> (a, Gen))

instance Functor GenUpdate where
  -- fmap :: (a -> b) -> GenUpdate a -> GenUpdate b
  fmap f (GenUpdate u) = GenUpdate $ \g -> let (x, g') = u g in (f x, g')

instance Applicative GenUpdate where
  -- pure :: a -> GenUpdate a
  pure x = GenUpdate $ \g -> (x, g)
  -- (<*>) :: GenUpdate (a -> b) -> GenUpdate a -> GenUpdate b
  GenUpdate uf <*> GenUpdate u = GenUpdate $ \g -> let (x, g') = u g ; (f, g'') = uf g' in (f x, g'')

instance Monad GenUpdate where
  -- (>>=) :: GenUpdate a -> (a -> GenUpdate b) -> GenUpdate b
  GenUpdate u >>= a_v = GenUpdate $ \g -> let (x, g') = u g ; GenUpdate v = a_v x in v g'
  -- (>>) :: GenUpdate a -> GenUpdate b -> GenUpdate b
  GenUpdate u >> GenUpdate v = GenUpdate $ \g -> let (x, g') = u g in v g'
  -- return :: a -> GenUpdate a
  return = pure

--
-- shuffle
--

shuffled_by :: [a] -> [Int] -> [a]
xs `shuffled_by` is = case (xs , is) of
  ([] , []   ) -> []
  ([x], _    ) -> [x]
  (xs , i:is') -> s:ss where
    (s, xs') = take_at i xs
    ss = xs' `shuffled_by` is'
    take_at i xs = (xs!!i, take i xs ++ drop (i+1) xs)

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

-- gen_index :: (Int, Int) -> GenUpdate Int
-- gen_index bounds = (randomR bounds :: Gen -> (Int, Gen))

gen_index :: (Int, Int) -> GenUpdate Int
gen_index = GenUpdate . randomR

gen_knuth_shuffle :: Int -> GenUpdate [Int]
gen_knuth_shuffle n =
  foldl f (return []) (map (\i -> gen_index (0, i)) [0..n])
    where f guis gui = do { is <- guis ; i <- gui ; return (i:is) }

shuffle :: [a] -> Gen -> [a]
shuffle xs g = do
  is <- gen_knuth_shuffle $ length xs
  xs `shuffled_by` 


  xs `shuffled_by` g
