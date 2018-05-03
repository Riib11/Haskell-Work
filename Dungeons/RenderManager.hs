{-# LANGUAGE TemplateHaskell #-}

module RenderManager
( window
, window_dim, window_dim_float, window_color
, worldwindow_dim, worldwindow_pos, worldwindow_size, worldtile_size
, sidewindow_dim, sidewindow_pos
, render
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Control.Lens
import GameManager
import GameObjects


{- /---------------------/
  /  window
 /---------------------/ -}

window :: Display
window =
    (InWindow
        "Dungeons"      -- window title
        window_dim      -- window size
        (10,  10))      -- window position

-- main window
window_dim          = (800,500)     :: (Int,Int)
window_dim_float    = (800,500)     :: Point
window_color        = green         :: Color

gray = makeColor 0.5 0.5 0.5 1

-- world window
worldwindow_dim     = (wws,wws)     :: Point
    where wws = worldwindow_size
worldwindow_pos     = (0,0)         :: Point
worldwindow_size    = 500           :: Float
worldtile_size      = 50            :: Float

-- TODO
-- side window
sidewindow_dim      = (300,500)     :: Point
sidewindow_pos      = (500,0)       :: Point


{- /---------------------/
  /  render
 /---------------------/ -}

render :: GameState -> Picture
render gs =
    translate x y
    $ Pictures
    $ world_pics ++ input_pics
    where world_pics = renderWorld w 0 0
          w         = getWorld gs
          x         = px + (ts/2) - (ww/2)
          y         = py + (wh/2) - (ts/2)
          ts        = worldtile_size
          (px,py)   = worldwindow_pos
          (ww,wh)   = window_dim_float
          input_pics = renderInputState inst
          inst      = getInputState gs

-- render world

renderWorld :: World -> Float -> Float -> [Picture]
renderWorld (World (t:[])) x y = tiles : []
    where tiles = Pictures $ renderTileArray t x y
renderWorld (World (t:ts)) x y = tiles : next
    where tiles = Pictures $ renderTileArray t x y
          next  = renderWorld (World ts) x (y-worldtile_size)

renderTileArray :: [WorldTile] -> Float -> Float -> [Picture]
renderTileArray []     x y = []
renderTileArray (t:ts) x y =
    (renderTile t x y) : (renderTileArray ts (x+worldtile_size) (y))

renderTile :: WorldTile -> Float -> Float -> Picture
renderTile tile x y = translate x y $ case tile of
    (TilePlayer player) -> renderSquare blue
    (TileEnemy  enemy ) -> renderSquare red
    (TileWall         ) -> renderSquare gray
    (TileEmpty        ) -> renderSquare black

renderSquare :: Color -> Picture
renderSquare c =
    color c $ rectangleSolid wts wts
    where wts = worldtile_size

-- render input state

cursor_dim          = (cursor_size,cursor_size)
cursor_size         = 25
cursor_color_idle   = orange
cursor_color_select = yellow

cursor :: Mouse -> Picture
cursor m =
    color c
    $ translate x y
    $ rectangleSolid w h
    where (x,y) = (-w,h)
          (w,h) = cursor_dim
          c = case mst of
            True -> cursor_color_select
            False -> cursor_color_idle
          mst = getMouseState m

renderInputState :: InputState -> [Picture]
renderInputState inst =
    (translate x y $ cursor m): []
    where (x,y)   = (mx+hw,my-hh)
          (hw,hh) = (ww/2,wh/2)
          (ww,wh) = window_dim_float
          (mx,my) = getMousePos m
          m       = getMouse inst