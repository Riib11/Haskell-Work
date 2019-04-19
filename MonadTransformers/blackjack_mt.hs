{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Functor.Identity
import System.Random
import Control.Lens
import Data.Char (toLower)

-----------------------------------------------------------------------------------------------------------------------------
-- playing cards
-----------------------------------------------------------------------------------------------------------------------------

data Card = Card { suite :: Suite, rank :: Rank }
data Suite = S_Spades | S_Clubs | S_Diamonds | S_Hearts deriving (Enum)
data Rank = R_Ace | R_2 | R_3 | R_4 | R_5 | R_6 | R_7 | R_8 | R_9 | R_10 | R_Jack | R_Queen | R_King deriving (Enum)

instance Show Suite where
  show suite = case suite of
    S_Spades   -> "S"
    S_Clubs    -> "C"
    S_Diamonds -> "D"
    S_Hearts   -> "H"

instance Show Rank where
  show rank = case rank of
    R_Ace   -> "A"
    R_10    -> "X"
    R_Jack  -> "J"
    R_Queen -> "Q"
    R_King  -> "K"
    _       -> show $ fromEnum rank + 1

instance Show Card where
  show card = show (rank card) ++ show (suite card)

deck_init = [ Card s r | s <- [S_Spades .. S_Hearts], r <- [R_Ace .. R_King] ]

-- score

data Score = Score { low_score :: Int, high_score :: Int }

instance Show Score where
  show score = (show . low_score) score ++ "/" ++ (show . high_score) score

rank_score :: Rank -> Score
rank_score rank = case rank of
  R_Ace   -> Score 1  11
  R_Jack  -> Score 10 10
  R_Queen -> Score 10 10
  R_King  -> Score 10 10
  _       -> Score r  r  where r = fromEnum rank + 1

card_score :: Card -> Score
card_score = rank_score . rank

score_add :: Score -> Score -> Score
score_add score1 score2 = Score (minimum possible_scores) (maximum possible_scores) where
  possible_scores = [ f score1 + g score2 | f <- [low_score, high_score], g <- [low_score, high_score]  ]

score_sum :: [Score] -> Score
score_sum scores = foldl score_add (head scores) (tail scores)

cards_score :: [Card] -> Score
cards_score = score_sum . map card_score

cards_value :: [Card] -> Int
cards_value cards = if high_score score <= 21 then high_score score else low_score score where
  score = cards_score cards

-- show

card_show :: Bool -> Card -> String
card_show hidden = if hidden then (\_ -> "##") else show

joined_by :: [String] -> String -> String
strings `joined_by` seperator = case strings of
  { [] -> ""; s:[] -> s; s:ss -> s ++ seperator ++ ss `joined_by` seperator }

hand_show :: Bool -> [Card] -> String
hand_show hidden cards = card_strings `joined_by` "," where
  card_strings = if hidden
    then (card_show True $ head cards) : (map (card_show False) $ tail cards)
    else (map (card_show False) cards)

-----------------------------------------------------------------------------------------------------------------------------
-- shuffle
-----------------------------------------------------------------------------------------------------------------------------

type SGState a = State StdGen a

shuffle :: [Int] -> [a] -> [a]
shuffle is xs = case xs of
  []  -> []
  [x] -> [x]
  xs  -> s_x:s_xs where
    (s_x, xs')   = extract (head is) xs
    s_xs         = shuffle (tail is) xs'
    extract i xs = (xs!!i, take i xs ++ drop (i+1) xs)

generate_knuth_shuffle_indices :: Int -> SGState [Int]
generate_knuth_shuffle_indices n = case n of
  0 -> return []
  _ -> do
    i  <- state $ randomR (0, n-1)
    is <- generate_knuth_shuffle_indices (n-1)
    return (i:is)

generate_shuffle :: [a] -> SGState [a]
generate_shuffle xs = do
  is <- generate_knuth_shuffle_indices (length xs)
  return $ shuffle is xs

shuffle_deck :: IO [Card]
shuffle_deck = do
  gen <- newStdGen
  return $ evalState (generate_shuffle deck_init) gen

-----------------------------------------------------------------------------------------------------------------------------
-- blackjack state
-----------------------------------------------------------------------------------------------------------------------------

-- Table

data Table = Table
  { _player :: [Card]
  , _dealer :: [Card]
  , _deck   :: [Card] }
  deriving (Show)

$(makeLenses ''Table)

table_init :: Table
table_init = Table [] [] []

-- display

display_table :: Bool -> String -> BJStateIO ()
display_table hidden label = do
  table <- get  
  lift . putStrLn $ (take 40 $ repeat '=') ++ " (" ++ label ++ ")"
  -- player's score
  lift . putStrLn $ "PLAYER: " ++ (show $ cards_value $ view player table) ++ " = " ++ hand_show False (view player table)
  -- dealer's score
  lift . putStr $ "DEALER: "
  if hidden
    then lift . putStr $ show (cards_value (drop 1 $ view dealer table)) ++ " < "
    else lift . putStr $ show (cards_value $ view dealer table)          ++ " = "
  lift . putStrLn $ hand_show hidden (view dealer table)

  

-----------------------------------------------------------------------------------------------------------------------------
-- blackjack updates
-----------------------------------------------------------------------------------------------------------------------------

-- BJState, BJStateT

type BJState   a = StateT Table Identity a
type BJStateIO a = StateT Table IO       a

-- lift from Identity to IO

liftID_IO :: Monad m => StateT s Identity a -> StateT s m a
liftID_IO st = StateT $ \s -> return (runIdentity $ runStateT st s) 

-- play

data Move = Stand | Hit

play_deal :: BJStateIO ()
play_deal = do
  liftID_IO deal_to_dealer
  liftID_IO deal_to_player
  liftID_IO deal_to_dealer
  liftID_IO deal_to_player
  display_table True "deal"

-- card manipulation

draw :: BJState Card
draw = do
  table <- get
  modify $ set deck (tail $ view deck table)
  return $ head (view deck table)

deal_to_dealer :: BJState ()
deal_to_dealer = do
  card <- draw
  table <- get
  modify $ set dealer (card : view dealer table)

deal_to_player :: BJState ()
deal_to_player = do
  card <- draw
  table <- get
  modify $ set player (card : view player table)

-- dealer play

play_dealer :: BJStateIO ()
play_dealer = do
  table <- get
  case cards_value (view dealer table) `compare` 21 of
    GT -> lift . putStrLn $ "> busted :("
    EQ -> lift . putStrLn $ "> nice 21!"
    LT -> do
      lift . putStrLn $ take 20 (repeat '=')
      move <- liftID_IO get_dealer_move
      case move of
        Stand -> do
          lift . putStrLn $ "dealer: stand"
          display_table False "dealer"
        Hit -> do
          lift . putStrLn $ "dealer: hit"
          liftID_IO deal_to_dealer
          display_table False "dealer"
          play_dealer

get_dealer_move :: BJState Move
get_dealer_move = do
  table <- get
  if cards_value (view player table) >= 21 || cards_value (view dealer table) >= 17
    then return Stand
    else return Hit

-- player play

play_player :: BJStateIO ()
play_player = do
  table <- get
  case cards_value (view player table) `compare` 21 of
    GT -> lift . putStrLn $ "> busted :("
    EQ -> lift . putStrLn $ "> nice 21!"
    LT -> do
      move <- lift get_player_move
      case move of
        Stand -> display_table False "player"
        Hit   -> do
          liftID_IO deal_to_player
          display_table True "player"
          play_player

get_player_move :: IO Move
get_player_move = do
  putStrLn $ take 20 (repeat '=')
  putStr "player: "
  input <- getLine
  if length input == 0 then putStrLn "please enter a move; moves are (h)it or (s)tand" >> get_player_move else
    if toLower (head input) == 'h' then return Hit else
      if toLower (head input) == 's' then return Stand
        else putStrLn "unrecognized move; moves are (h)it or (s)tand" >> get_player_move

-- win

display_winner :: BJStateIO ()
display_winner = do
  lift . putStrLn $ take 20 (repeat '=')
  table <- get
  let (p, d) = (cards_value $ view player table, cards_value $ view dealer table)
  if p <= 21 && (d > 21 || d < p)
    then lift $ putStrLn "> you win!"
    else lift $ putStrLn "> you lose..."

-----------------------------------------------------------------------------------------------------------------------------
-- main
-----------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = fmap fst $ runStateT play table_init

play :: BJStateIO ()
play = do
  lift . putStrLn $ take 70 (repeat '=')
  lift . putStrLn $ "staring round..."

  -- shuffle deck
  put table_init
  cards <- lift shuffle_deck
  modify $ set deck cards

  -- deal first two cards to each of player and dealer
  play_deal
  play_player
  play_dealer

  -- display winner
  display_winner

  -- repeat or end
  again <- lift get_playagain
  if again
    then play
    else lift $ putStrLn "quitting blackjack..."

-- play again?

get_playagain :: IO Bool
get_playagain = do
  putStr "Play again? "
  input <- getLine
  if (toLower . head) input == 'y' then return True
    else if (toLower . head) input == 'n' then return False
     else putStrLn "unrecognized response, please answer (y)es or (n)o" >> get_playagain
