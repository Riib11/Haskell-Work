import GameObjects

tile_player = TilePlayer default_player

world = World
    [ [TileWall,TileEmpty,TileWall]
    , [TileEmpty,tile_player,TileWall]
    , [TileWall,TileEmpty,TileWall] ]