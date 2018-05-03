{-# LANGUAGE TemplateHaskell #-}

module InputManager
( input
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Control.Lens
import GameManager
import GameObjects

{- /---------------------/
  /  input
 /---------------------/ -}

input :: Event -> GameState -> GameState
input evt gs =
    let inst   = getInputState gs
        (kb,m) = (getKeyboard inst, getMouse inst)
        newGameState new_inst = setInputState new_inst gs
    in newGameState $ case evt of
        -- mouse click
        (EventKey (MouseButton LeftButton) state modifiers pt)
            -> setMouse new_m inst
                where new_m   = setMouseState new_mst m
                      new_mst = case state of
                        Down -> True
                        Up   -> False
        -- mouse move
        (EventMotion pt)
            -> setMouse new_mouse inst
                where new_mouse = setMousePos pt m
        -- keyboard
        (EventKey key state modifiers pt)
            -> setKeyboard (handle_kp kp kb) inst
                where handle_kp = case state of
                        Down -> addKeyPress    -- a key is pressed
                        Up   -> removeKeyPress -- a key is released
                      kp = (key,modifiers,pt)
        -- other
        (EventResize v)
            -> inst


-- World

bounded :: Float -> Float -> Float -> Bool
bounded min max x = min <= x && x <= max

bounded2D :: Point -> Point -> Point -> Bool
bounded2D (xm,ym) (xM,yM) (x,y) =
    (bounded xm xM x) && (bounded ym yM y)

getSelectedTile :: World -> Mouse -> WorldTile
getSelectedTile (World tiles) = helper (0,0) tiles
    where helper p (t:ts) = if bounded2D tm tM p
            then t else getSelectedTile