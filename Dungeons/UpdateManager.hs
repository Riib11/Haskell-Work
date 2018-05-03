module UpdateManager
( update
, rate
) where

import Graphics.Gloss.Interface.Pure.Game
import GameManager
import GameObjects

-- updates per second
rate :: Int
rate = 5

update :: Float -> GameState -> GameState
update dt gs =
    updateInputState gs