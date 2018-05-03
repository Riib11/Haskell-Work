{-# LANGUAGE TemplateHaskell #-}

module GameManager
( GameState(GameState)
, getInputState,setInputState
, getPlayer, setPlayer
, getWorld, setWorld
, initGameState
, InputState(InputState)
, getMouse, setMouse
, getKeyboard, setKeyboard
, start_inputstate
, updateInputState
, Mouse(Mouse)
, getMousePos, setMousePos
, getMouseState, setMouseState
, start_mouse
, KeyPress
, Keyboard(Keyboard)
, getKeysPressed, setKeysPressed
, addKeyPress, compareKeys, removeKeyPress
, getKeysDown, setKeysDown
, addKeyDown
, start_keyboard
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Utilities
import Control.Lens
import GameObjects


{- /---------------------/
  /  InputState
 /---------------------/ -}

-- Mouse

data Mouse = Mouse
    { _mouse_pos     :: Point
    , _mouse_state   :: Bool
    } deriving (Show)
$(makeLenses ''Mouse)

(getMousePos   , setMousePos   ) = (view mouse_pos, set mouse_pos) :: (Mouse -> Point, Point -> Mouse -> Mouse)
(getMouseState , setMouseState ) = (view mouse_state, set mouse_state) :: (Mouse -> Bool, Bool -> Mouse -> Mouse)

start_mouse = Mouse
    { _mouse_pos = (0.0,0.0)
    , _mouse_state = False }

-- Keyboard

type KeyPress = (Key,Modifiers,Point)

-- TODO: keys_released ?
data Keyboard = Keyboard
    { _keys_pressed :: [KeyPress]
    , _keys_down  :: [KeyPress]
    } deriving (Show)
$(makeLenses ''Keyboard)

(getKeysDown   , setKeysDown   ) = (view keys_down, set keys_down) :: (Keyboard -> [KeyPress], [KeyPress] -> Keyboard -> Keyboard)
(getKeysPressed, setKeysPressed) = (view keys_down, set keys_down) :: (Keyboard -> [KeyPress], [KeyPress] -> Keyboard -> Keyboard)

addKeyPress :: KeyPress -> Keyboard -> Keyboard
addKeyPress k kb = setKeysPressed (k:ks) kb
    where ks = getKeysPressed kb

compareKeys :: KeyPress -> KeyPress -> Bool
compareKeys (k1,m1,_) (k2,m2,_) =
    (k1==k2) && (m1==m2)

removeKeyPress :: KeyPress -> Keyboard -> Keyboard
removeKeyPress kp kb = kb2
    where kb2       = setKeysDown new_kds kb1
          kb1       = setKeysPressed new_kps kb
          new_kps   = filter not_key kps -- remove kp from kps
          new_kds   = filter not_key kds -- remove 
          (kps,kds) = (getKeysPressed kb, getKeysDown kb)
          not_key   = not . (compareKeys kp)

addKeyDown :: KeyPress -> Keyboard -> Keyboard
addKeyDown k kb = setKeysDown (k:ks) kb
    where ks = getKeysDown kb

-- update
--      add all keyspressed to keysdown
--      reset keyspressed
updateKeyboard :: Keyboard -> Keyboard
updateKeyboard kb = reset_kps $ update_kds kb
    where reset_kps  = setKeysPressed []
          update_kds = setKeysDown new_kds
          new_kds    = setUnion compareKeys kps kds
          (kps,kds)  = (getKeysPressed kb, getKeysDown kb)

start_keyboard = Keyboard
    { _keys_pressed = [] :: [KeyPress]
    , _keys_down    = [] :: [KeyPress] }

-- InputState

data InputState = InputState
    { _mouse    :: Mouse
    , _keyboard :: Keyboard
    } deriving (Show)
$(makeLenses ''InputState)

(getMouse   , setMouse   )  = (view mouse, set mouse) :: (InputState -> Mouse, Mouse -> InputState -> InputState)
(getKeyboard, setKeyboard) = (view keyboard, set keyboard) :: (InputState -> Keyboard, Keyboard -> InputState -> InputState)

start_inputstate = InputState
    { _mouse    = start_mouse
    , _keyboard = start_keyboard }

{- /---------------------/
  /  GameState
 /---------------------/ -}

data GameState = GameState
    { _input_state  :: InputState
    , _player       :: Player
    , _world        :: World
    } deriving (Show)
$(makeLenses ''GameState)

(getInputState, setInputState) = (view input_state, set input_state) :: (GameState -> InputState, InputState -> GameState -> GameState)
(getPlayer    , setPlayer    ) = (view player, set player) :: (GameState -> Player, Player -> GameState -> GameState)
(getWorld     , setWorld     ) = (view world, set world) :: (GameState -> World, World -> GameState -> GameState)

initGameState :: Player -> World -> GameState
initGameState p w = GameState
    { _input_state = start_inputstate
    , _player      = p
    , _world       = w }

updateInputState :: GameState -> GameState
updateInputState gs = setInputState new_inst gs
    where new_inst = setKeyboard new_kb inst
          new_kb   = updateKeyboard kb
          kb       = getKeyboard inst
          inst     = getInputState gs