-- file: bjk.hs
-- author: Jim Fix (Reed College), 2019

import System.Random
import Control.Monad

import Card

type Hand = [Card]
type CardStack = [Card]
fullDeck  = [Card r s | s <- suits, r <- ranks] :: CardStack
data BlackjackState = BlackjackState { player :: Hand, house :: Hand, stack :: CardStack }


-- -- -- --
scoreHand :: Hand -> Int
--
scoreHand hand =
    if lowScore <= 11 && numAces > 0 then lowScore + 10 else lowScore
    where smallScore hand = sum (map worth hand)
          lowScore = smallScore hand
          aceCount hand = sum [1 | c <- hand, isAce c]
          numAces = aceCount hand

          
-- -- -- --
hiddenCard :: Card -> String
--
hiddenCard _ = "### "

-- -- -- --
showHand :: Hand -> String
--
showHand cs = foldr (++) "" (map showCard cs)

-- -- -- --
showHandIf :: Bool -> Hand -> String
--
showHandIf showAll (c:cs) =
  if showAll
  then (foldr (++) "" (map showCard (c:cs)))
  else (showCard c) ++ (foldr (++) "" (map hiddenCard cs))


-- -- -- --
showState :: BlackjackState -> Bool -> String
--
showState bjs midGame = "YOU: " ++ (showHand (player bjs)) ++ " THEM: " ++ (showHandIf (not midGame) (house bjs))


-- -- -- --
revealState :: BlackjackState -> IO ()
--
revealState s = putStrLn (showState s False);


-- -- -- --
hideState :: BlackjackState -> IO ()
--
hideState s = putStrLn (showState s True);


data StdGenConsumer a = StdGenConsumer (StdGen -> (a, StdGen))

-- -- -- --
consumedBy :: StdGen -> StdGenConsumer a -> (a, StdGen)
--
g `consumedBy` (StdGenConsumer u) = u g

instance Functor StdGenConsumer where
  fmap f (StdGenConsumer u) = StdGenConsumer (\g -> let (x,g') = u g in (f x,g'))

instance Applicative StdGenConsumer where
  pure x = StdGenConsumer (\g -> (x,g))
  StdGenConsumer fu <*> StdGenConsumer u = StdGenConsumer (\g0 -> let (f,g1) = fu g0 in let (x,g2) = u g1 in (f x,g2))

instance Monad StdGenConsumer where
  return = pure
  move >>= nextMove = StdGenConsumer (\g0 -> let (info, g1) = g0 `consumedBy` move in g1 `consumedBy` (nextMove info))

-- -- -- --
shuffledBy :: [a] -> [Int] -> [a]
--
[]  `shuffledBy` _      = []
[x] `shuffledBy` _      = [x]
xs  `shuffledBy` (r:rs) = (x:rxs)
   where (x,xs') = extract r xs
         rxs     = xs' `shuffledBy` rs
         extract i xs = (xs !! i, (take i xs) ++ (drop (i+1) xs))

-- -- -- --
knuthShuffleIndices :: Int -> StdGenConsumer [Int]
--
knuthShuffleIndices 0   = return []
knuthShuffleIndices num = do {
  r  <- StdGenConsumer $ randomR (0,num-1);
  rs <- knuthShuffleIndices (num-1);
  return (r:rs)
}

-- -- -- --
shuffle :: [a] -> StdGen -> [a]
--
shuffle xs g = xs `shuffledBy` (fst (g `consumedBy` (knuthShuffleIndices (length xs))))

-- -- -- --
getShuffledDeck :: IO CardStack
--
getShuffledDeck =
  do {
    gen <- newStdGen;
    return $ shuffle fullDeck gen
  }

--
-- game state update
--

type BlackjackUpdate a = BlackjackState -> (a, BlackjackState)

just bjkState = ((),bjkState)

data BlackjackUpdater a = BlackjackUpdater (BlackjackState -> (a, BlackjackState))

-- -- -- --
updatedWith ::  BlackjackState -> BlackjackUpdater a -> (a, BlackjackState)
--
s `updatedWith` (BlackjackUpdater u) = u s

instance Functor BlackjackUpdater where
  fmap f (BlackjackUpdater u) = BlackjackUpdater (\s -> let (x,s') = u s in (f x,s'))

instance Applicative BlackjackUpdater where
  pure x = BlackjackUpdater (\s -> (x,s))
  BlackjackUpdater fu <*> BlackjackUpdater u =  BlackjackUpdater (\s0 -> let (f,s1) = fu s0 in let (x,s2) = u s1 in (f x,s2))

instance Monad BlackjackUpdater where
  return x = BlackjackUpdater (\s -> (x,s))
  move >>= nextMove = BlackjackUpdater (\s0 -> let (info, s1) = s0 `updatedWith` move in s1 `updatedWith` (nextMove info))


-- -- -- --
bjkDo :: BlackjackUpdate a -> BlackjackUpdater a
--
bjkDo u = BlackjackUpdater u


-- -- -- --
freshGame :: CardStack -> BlackjackState
--
freshGame d = BlackjackState {player=[], house=[], stack=d}


-- -- -- --
dealToPlayer :: BlackjackUpdate ()
--
dealToPlayer (BlackjackState {player=p, house=h, stack=c:cs})
                 = just (BlackjackState {player=c:p, house=h, stack=cs})
dealToPlayer bjs = just bjs


-- -- -- --
dealToHouse :: BlackjackUpdate ()
--
dealToHouse (BlackjackState {player=p, house=h, stack=c:cs})
                = just (BlackjackState {player=p, house=c:h, stack=cs})
dealToHouse bjs = just bjs


-- -- -- --
hold :: BlackjackUpdate ()
--
hold bjs = just bjs


-- -- -- --
getHouse :: BlackjackUpdate [Card]
--
getHouse bjs = (house bjs,bjs)


-- -- -- --
getPlayer :: BlackjackUpdate [Card]
--
getPlayer bjs = (player bjs,bjs)


-- -- -- --
housePlay :: BlackjackUpdater ()
--
housePlay = do {
  h <- bjkDo $ getHouse;
  p <- bjkDo $ getPlayer;
  if scoreHand p >= 21 || (scoreHand h >= 17) then do {
    bjkDo $ hold
  } else do {
    bjkDo $ dealToHouse;
    housePlay
  }
}


-- -- -- -- 
initialPlay :: BlackjackUpdater ()
--
initialPlay =
  do {
    bjkDo $ dealToPlayer;
    bjkDo $ dealToHouse;
    bjkDo $ dealToPlayer;
    bjkDo $ dealToHouse;
    return ()
  }


-- -- -- --
bjkIOresult :: BlackjackState -> BlackjackUpdater a -> IO (a, BlackjackState)
--
bjkIOresult s u = return (s `updatedWith` u)


-- -- -- --
bjkIOupdate :: BlackjackState -> BlackjackUpdater a -> IO BlackjackState
--
bjkIOupdate s u = return (snd (s `updatedWith` u))

                  
-- -- -- --
hitOrStand :: BlackjackState -> IO Bool
--
hitOrStand s = if scoreHand (player s) == 21 then do {
  putStrLn "Twenty-one! We assume you want to stand.";
  return True
} else do {
  putStrLn "(H)it or (S)tand?";
  answer <- getLine;
  return (answer == "s" || answer == "S")
}


-- -- -- --
getPlayAgain :: IO Bool
--
getPlayAgain = do {
  putStrLn "Play again? (Y)es or (N)o.";
  answer <- getLine;
  return (answer == "y" || answer == "Y")
}


-- -- -- --
playerPlay :: BlackjackState -> IO BlackjackState
--
playerPlay s = do {
  stand <- hitOrStand s;
  if stand then do {
    return s
  } else do {
    s' <- s `bjkIOupdate` (BlackjackUpdater dealToPlayer);
    hideState s';
    if (scoreHand (player s')) > 21 then do {
      putStrLn "You went BUST!";
      return s'
    } else do {
      playerPlay s'
    }
  }
}                      

beatsHouseOf :: Int -> Int -> Bool
pScore `beatsHouseOf` hScore = pScore <= 21 && hScore > 21 || pScore <= 21 && hScore < pScore

reportWinner :: BlackjackState -> IO ()
reportWinner bs
 | pScore `beatsHouseOf` hScore = do {
     putStrLn "You win!"
   }
 | hScore /= 21 && hScore == pScore = do {
     putStrLn "Push."
   }
 | otherwise = do {
     putStrLn "Sorry. The house wins."
   }
 where pScore = scoreHand (player bs)
       hScore = scoreHand (house bs)

--
-- driver
--

main = do {

  --
  -- Shuffle the deck then deal two cards each to the player's and house hands.
  bsFresh <- fmap freshGame getShuffledDeck;
  bsDealt <- bsFresh `bjkIOupdate` initialPlay;
  hideState bsDealt;

  --
  -- Let the player play out their hand
  bsAfterPlay <- playerPlay bsDealt;
  revealState bsAfterPlay;

  --
  -- Watch the house's hand unfurl
  bsEnd <- bsAfterPlay `bjkIOupdate` housePlay;
  revealState bsEnd;

  --
  -- Report who wins.
  reportWinner bsEnd;

  --
  -- Repeat or end.
  again <- getPlayAgain;
  if again then main else return ()
}

