-- file: bjk.hs
-- author: Jim Fix (Reed College), 2019

import System.Random
import Control.Monad

import Card

type Hand = [Card]
type CardStack = [Card]
fullDeck  = [Card r s | s <- suits, r <- ranks] :: CardStack
data State = State { player :: Hand, house :: Hand, stack :: CardStack }


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
showState :: State -> Bool -> String
--
showState s midGame = "YOU: " ++ (showHand (player s)) ++ " THEM: " ++ (showHandIf (not midGame) (house s))


-- -- -- --
revealState :: State -> IO ()
--
revealState s = putStrLn (showState s False);


-- -- -- --
hideState :: State -> IO ()
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

f .> g = g . f

type Update a   = State -> (a, State)
data MUpdate a  = MUpdate (Update a)

updateOf (MUpdate u) = u

no_value s = ((),s)
no_update x = (\s -> (x,s))
update_map f u = (\s -> let (x,s') = u s in (f x, s'))
update_app uf ux = (\s -> let (f,s1) = uf s in let (x,s2) = ux s1 in (f x, s2))
update_bind x_u u = (\s -> let (x,s') = u s in (x_u x) s')
u `fedTo` x_u = update_bind x_u u

instance Functor MUpdate where
  fmap f  = updateOf .> (update_map f) .> MUpdate

instance Applicative MUpdate where
  pure = no_update .> MUpdate
  (<*>) (MUpdate uf) = updateOf .> (update_app uf) .> MUpdate 

instance Monad MUpdate where
  return = pure
  (>>=) (MUpdate u) x_mu = MUpdate $ \s -> let (x,s') = u s in (updateOf (x_mu x)) s'


doUpdate :: Update a -> MUpdate a
doUpdate = MUpdate

updateDoneBy :: MUpdate a -> Update a
updateDoneBy = updateOf

freshGame :: CardStack -> State
freshGame d = State {player=[], house=[], stack=d}

drawNextCard :: Update Card
drawNextCard (State {player=p, house=h, stack=c:cs}) = (c,State {player=p, house=h, stack=cs})

givePlayer :: Card -> Update ()
givePlayer c (State {player=p, house=h, stack=cs}) = no_value $ State {player=c:p, house=h, stack=cs}

giveHouse :: Card -> Update ()
giveHouse c (State {player=p, house=h, stack=cs}) = no_value $ State {player=p, house=c:h, stack=cs}

dealToPlayer :: Update ()
dealToPlayer = drawNextCard `fedTo` givePlayer

dealToHouse:: Update ()
dealToHouse = drawNextCard `fedTo` giveHouse

getUsing :: (State -> a) -> Update a
getUsing extractor s = (extractor s,s)

hold :: Update ()
hold = no_update ()

getHouse :: Update [Card]
getHouse = getUsing house

getPlayer :: Update [Card]
getPlayer = getUsing player

housePlay :: Update ()
housePlay = updateDoneBy $ do {
  h <- doUpdate $ getHouse;
  p <- doUpdate $ getPlayer;
  if scoreHand p >= 21 || (scoreHand h >= 17) then do {
    doUpdate $ hold
  } else do {
    doUpdate $ dealToHouse;
    doUpdate $ housePlay
  }
}

initialPlay :: Update ()
initialPlay = updateDoneBy $
  do {
    doUpdate $ dealToPlayer;
    doUpdate $ dealToHouse;
    doUpdate $ dealToPlayer;
    doUpdate $ dealToHouse
  }


--
-- game state update w/ interactivity
--

type IOUpdate a = State -> IO (a, State)
data MIOUpdate a = MIOUpdate (IOUpdate a)

ioUpdateOf (MIOUpdate iou) = iou
ioUpdateDoneBy = ioUpdateOf
doIOUpdate = MIOUpdate

instance Functor MIOUpdate where
  fmap f (MIOUpdate iou) = MIOUpdate $ \s -> do (x,s') <- iou s
                                                return (f x, s')

instance Applicative MIOUpdate where
  pure x = MIOUpdate $ \s -> return (x,s) 
  (MIOUpdate iouf) <*> (MIOUpdate ioux) = MIOUpdate $ \s -> do (f,s1) <- iouf s
                                                               (x,s2) <- ioux s1
                                                               return (f x, s2)
instance Monad MIOUpdate where
  return = pure
  (MIOUpdate iou) >>= x_iou = MIOUpdate $ \s -> do (x,s') <- iou s
                                                   let (MIOUpdate iou') = x_iou x
                                                   iou' s'



getWith :: (State -> a) -> MIOUpdate a
getWith extractor = MIOUpdate $ \s -> return (extractor s,s)                                               

actWith :: IO a -> MIOUpdate a
actWith io = MIOUpdate $ \s -> do {
   x <- io;
   return (x,s)
}

updateWith :: Update a -> MIOUpdate a
updateWith u = MIOUpdate $ \s -> return $ u s

reportWith :: (State -> IO a) -> MIOUpdate a
reportWith sa = MIOUpdate $ \s -> do { x <- sa s; return (x,s) }
  
                  
wantsToHit :: IOUpdate Bool
wantsToHit = ioUpdateDoneBy $ do {
  p <- getWith $ player;
  if  scoreHand p == 21 then do {
    actWith $ putStrLn "Twenty-one! We assume you want to stand.";
    return False
  } else do {
    actWith $ putStrLn "(H)it or (S)tand?";
    answer <- actWith $ getLine;
    return (answer == "h" || answer == "H")
  }
} 

playerPlay :: IOUpdate ()
playerPlay = ioUpdateDoneBy $ do {
  hit <- doIOUpdate $ wantsToHit;
  if not hit then do {
    updateWith $ hold
  } else do {
    updateWith $ dealToPlayer;
    reportWith $ hideState;
    p <- getWith $ player;
    if (scoreHand p) > 21 then do {
      actWith $ putStrLn "You went BUST!";
    } else do {
      doIOUpdate $ playerPlay
    }
  }
}                      

beatsHouseOf :: Int -> Int -> Bool
pScore `beatsHouseOf` hScore = pScore <= 21 && hScore > 21 || pScore <= 21 && hScore < pScore

reportWinner :: State -> IO ()
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

playOneGame :: IOUpdate ()
playOneGame = ioUpdateDoneBy $ do {
  updateWith $ initialPlay;
  reportWith $ hideState;
  doIOUpdate $ playerPlay;
  reportWith $ revealState;
  updateWith $ housePlay;
  reportWith $ revealState;
  reportWith $ reportWinner;
} 

wantsToPlayAgain :: IO Bool
wantsToPlayAgain = do {
  putStrLn "Play again? (Y)es or (N)o.";
  answer <- getLine;
  return (answer == "y" || answer == "Y")
}

--
-- driver
--

main = do {
  newDeck <- getShuffledDeck;
  playOneGame $ freshGame newDeck;

  --
  -- Repeat or end.
  again <- wantsToPlayAgain;
  if again then main else return ()
}
