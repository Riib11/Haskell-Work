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

instance Show Card where
  show c = show (suite c) ++ ":" ++ show (rank c)

data Suite
  = S_Spades | S_Clubs | S_Diamonds | S_Hearts
  deriving (Eq, Enum)

instance Show Suite where
  show S_Spades   = "S"
  show S_Clubs    = "C"
  show S_Diamonds = "D"
  show S_Hearts   = "H"

data Rank
  = R_Ace | R_2 | R_3 | R_4 | R_5 | R_6 | R_7 | R_8 | R_9 | R_10 | R_Jack | R_Queen | R_King
  deriving (Eq, Enum)

instance Show Rank where
  show R_Ace   = "A"
  show R_Jack  = "J"
  show R_Queen = "Q"
  show R_King  = "K"
  show rank    = (show . (+1) . fromEnum) rank

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

data Score = Score
  { low_score :: Int, high_score :: Int }
  deriving (Show)

get_hand_score :: Card -> Score
get_hand_score c = case rank c of
  R_Ace   -> Score 1  11
  R_Jack  -> Score 10 10
  R_Queen -> Score 10 10
  R_King  -> Score 10 10
  rank    -> Score r  r  where r = 1 + fromEnum rank

sum_scores :: [Score] -> Score
sum_scores ss = Score (total low_score) (total high_score)
  where total get_hand_score = sum $ map get_hand_score ss

get_hand_total :: Hand -> Int
get_hand_total hand = if high_score score <= 21 then high_score score else low_score score
  where score = sum_scores $ map get_hand_score hand

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
  divider
  where divider = (take 40 $ repeat '-')

display_state :: Bool -> BJState -> IO ()
display_state hidden s = putStrLn $ show_state hidden s

-----------------------------------------------------------------------------------------------------------------------------
--  Shuffle
-----------------------------------------------------------------------------------------------------------------------------

--
-- StdGenConsumer
--

newtype StdGenConsumer a = StdGenConsumer (StdGen -> (a, StdGen))

consumed_by :: StdGen -> StdGenConsumer a -> (a, StdGen)
g `consumed_by` StdGenConsumer x = x g

instance Functor StdGenConsumer where
  -- fmap :: (a -> b) -> StdGenConsumer a -> StdGenConsumer b
  fmap f (StdGenConsumer x) = StdGenConsumer fx where
    fx g = let (x', g') = x g in (f x', g')

instance Applicative StdGenConsumer where
  -- pure :: a -> StdGenConsumer a
  pure x = StdGenConsumer $ \g -> (x, g)
  -- (<*>) :: StdGenConsumer (a -> b) -> StdGenConsumer a -> StdGenConsumer b
  StdGenConsumer f <*> StdGenConsumer x = StdGenConsumer fx where
    fx g = let (h, g') = f g ; (x', g'') = x g' in (h x', g'')

instance Monad StdGenConsumer where
  -- return :: a -> StdGenConsumer a
  return = pure
  -- (>>=) :: StdGenConsumer a -> (a -> StdGenConsumer b) -> StdGenConsumer b
  before >>= now = StdGenConsumer after where after g = let (x, g') = g `consumed_by` before in g' `consumed_by` (now x)

--
-- shuffle
--

shuffled_with :: [a] -> [Int] -> [a]
shuffled_with xs rs = case (xs, rs) of
  ([],  _   ) -> []
  ([x], _   ) -> [x]
  (xs,  i:rs) -> x:rxs where
    (x,xs') = extract i xs
    rxs = xs' `shuffled_with` rs
    extract i xs = (xs!!i, take i xs ++ drop (i+1) xs)

knuth_shuffle_indices :: Int -> StdGenConsumer [Int]
knuth_shuffle_indices 0 = return []
knuth_shuffle_indices n = do
  r  <- StdGenConsumer $ randomR (0, n-1)
  rs <- knuth_shuffle_indices (n-1)
  return $ r:rs


shuffle :: [a] -> StdGen -> [a]
shuffle xs g = xs `shuffled_with` (fst $ g `consumed_by` knuth_shuffle_indices (length xs))

shuffled_deck :: IO Deck
shuffled_deck = do
  g <- newStdGen
  return $ shuffle deck_init g

-----------------------------------------------------------------------------------------------------------------------------
--  Update
-----------------------------------------------------------------------------------------------------------------------------

type    BJUpdate  a = BJState -> (a, BJState)
newtype BJUpdater a = BJUpdater (BJUpdate a)

updated_by :: BJState -> BJUpdater a -> (a, BJState)
updated_by s (BJUpdater u) = u s

instance Functor BJUpdater where
  fmap f (BJUpdater u) = BJUpdater fu where
    fu s = let (x, s') = u s in (f x, s')

instance Applicative BJUpdater where
  pure x = BJUpdater (\s -> (x, s))
  BJUpdater uf <*> BJUpdater u = BJUpdater fu where
    fu s = let (f, s') = uf s ; (x, s'') = u s' in (f x, s'')

instance Monad BJUpdater where
  return = pure
  before >>= now = BJUpdater after where after s = let (x, s') = s `updated_by` before in s' `updated_by` (now x)

-----------------------------------------------------------------------------------------------------------------------------
--  Update
-----------------------------------------------------------------------------------------------------------------------------

update_id :: BJState -> ((), BJState)
update_id s = ((), s)

update_via :: BJUpdate a -> BJUpdater a
update_via u = BJUpdater u

restart :: Deck -> BJState
restart cs = set deck cs init_state

--
-- deal
--

deal_to target target_ s = update_id $ (set target (c : view target_ s) . set deck cs) s where (c:cs) = view deck s

deal_to_player :: BJUpdate ()
deal_to_player = deal_to player player

deal_to_dealer :: BJUpdate ()
deal_to_dealer = deal_to dealer dealer

--
-- move
--

hold :: BJUpdate ()
hold = update_id

--
-- getters
--

get_dealer :: BJUpdate [Card]
get_dealer s = (view dealer s, s)

get_player :: BJUpdate [Card]
get_player s = (view player s, s)

--
-- play
--


play_house :: BJUpdater ()
play_house = do
  d <- update_via get_dealer
  p <- update_via get_player
  if get_hand_total p >= 21 || get_hand_total d >= 17
    then update_via hold
    else update_via deal_to_dealer >> play_house

play_init :: BJUpdater ()
play_init =
  foldl (>>) (update_via update_id) $
  map update_via $ 
  take 4 $ cycle [deal_to_player, deal_to_dealer]

--
-- IO
--

update_result_io :: BJState -> BJUpdater a -> IO (a, BJState)
update_result_io s u = return $ s `updated_by` u

update_io :: BJState -> BJUpdater a -> IO BJState
update_io s u = fmap snd (update_result_io s u)

--
-- play
--

data Move = Hit | Stand

read_move :: String -> Maybe Move
read_move "" = Nothing
read_move (c:_) = let c_ = toLower c in case c_ of
  'h' -> Just Hit
  's' -> Just Stand
  _   -> Nothing

get_player_move :: BJState -> IO Move
get_player_move s = if get_hand_total (view player s) == 21
  then putStrLn "21! You will surely stand." >> return Stand
  else do
    putStr "player's move:\n> "
    input <- getLine
    case read_move input of
      Just move -> return move
      Nothing   -> putStrLn "That's an unrecognized move..." >> get_player_move s

get_playagain :: IO Bool
get_playagain = do
  putStr "Play again?\n> "
  playagain <- getLine
  return $ (toLower . head) playagain == 'y'

play_player :: BJState -> IO BJState
play_player s = do
  move <- get_player_move s
  case move of
    Stand -> return s
    Hit   -> do
      s' <- s `update_io` update_via deal_to_player
      display_state False s' -- TODO: this is `hideState` in Jim's code
      if get_hand_total (view player s') > 21
        then putStrLn "player busted :(" >> return s'
        else play_player s'

--
-- win, lose
--

check_player_win :: BJState -> Bool
check_player_win s = p <= 21 && (d > 21 || d < p) where (p,d) = get_hand_totals s

check_tie :: BJState -> Bool
check_tie s = p <= 21 && p == d where (p,d) = get_hand_totals s

display_winner :: BJState -> IO ()
display_winner s
  | check_player_win s = putStrLn "You win!"
  | check_tie s        = putStrLn "Its a tie."
  | otherwise          = putStrLn "House wins."


-----------------------------------------------------------------------------------------------------------------------------
--  Main
-----------------------------------------------------------------------------------------------------------------------------

main = do

  putStrLn $ take 70 $ repeat '='
  
  -- shuffle deck
  cs <- shuffled_deck
  let s_shuffled = set deck cs init_state
  
  -- deal first hand
  s_dealt <- s_shuffled `update_io` play_init
  display_state True s_dealt

  -- player plays hand
  s_played <- play_player s_dealt
  display_state True s_played

  -- dealer plays hand
  s_ended <- s_played `update_io` play_house
  display_state False s_ended

  -- display result
  display_winner s_ended

  -- repeat or end
  again <- get_playagain
  if again then main else return ()
