import Graphics.Gloss.Interface.Pure.Game
import GameManager
import GameObjects
import InputManager 
import RenderManager
import UpdateManager


-- main
main = play window window_color rate start_gamestate render input update

-- start game state
start_gamestate = initGameState
    player
    (World
        [ [ TileWall   , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ TileEmpty  , tile_player , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileEmpty , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ tile_enemy , TileEmpty   , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileWall , TileEmpty ]
        , [ TileWall   , TileEmpty   , TileWall , TileWall , tile_enemy , TileWall , TileWall , TileWall , TileWall , TileEmpty ] ])
    where tile_player = TilePlayer player
          player      = default_player
          tile_enemy  = TileEnemy enemy
          enemy       = default_enemy