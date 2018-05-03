{-# LANGUAGE TemplateHaskell #-}

module GameObjects
( World(World)
, WorldTile(TilePlayer,TileEnemy,TileWall,TileEmpty)
, Player(Player)
, Enemy(Enemy)
, Damage(Damage)
, DamageType(PhysicalDamage,MagicDamage,TrueDamage)
, default_player, default_enemy, default_entity_stats, default_damage
) where

import Control.Lens


{- /---------------------/
  /  Stats
 /---------------------/ -}

data EntityStats = EntityStats
    { _name   :: String
    , _health :: Int
    , _level  :: Int
    } deriving (Show)
$(makeLenses ''EntityStats)

(getName  , setName  ) = (view name, set name) :: (EntityStats -> String, String -> EntityStats -> EntityStats)
(getHealth, setHealth) = (view health, set health) :: (EntityStats -> Int, Int -> EntityStats -> EntityStats)
(getLevel , setLevel ) = (view level, set level) :: (EntityStats -> Int, Int -> EntityStats -> EntityStats)

data DamageType =
      PhysicalDamage
    | MagicDamage
    | PoisonDamae
    | TrueDamage
    deriving (Show)

data Damage = Damage
    { _amount        :: Int
    , _damage_type   :: DamageType
    } deriving (Show)
$(makeLenses ''Damage)

(getAmount    , setAmount    ) = (view amount, set amount) :: (Damage -> Int, Int -> Damage -> Damage)
(getDamageType, setDamageType) = (view damage_type, set damage_type) :: (Damage -> DamageType, DamageType -> Damage -> Damage)


{- /---------------------/
  /  Player
 /---------------------/ -}

data Player = Player
    { _player_stats   :: EntityStats
    , _player_damage  :: Damage
    } deriving (Show)
$(makeLenses ''Player)


{- /---------------------/
  /  Enemy
 /---------------------/ -}

data Enemy = Enemy
    { _enemy_stats    :: EntityStats
    , _enemy_damage   :: Damage
    } deriving (Show)
$(makeLenses ''Enemy)

(getEnemyStats , setEnemyStats ) = (view enemy_stats, set enemy_stats) :: (Enemy -> EntityStats, EntityStats -> Enemy -> Enemy)
(getEnemyDamage, setEnemyDamage) = (view enemy_damage, set enemy_damage) :: (Enemy -> Damage, Damage -> Enemy -> Enemy)

{- /---------------------/
  /  World
 /---------------------/ -}

data World = World [[WorldTile]]

instance Show World where
    show (World ((x:[]):[])) = show x
    show (World ((x:xs):[])) = show x
        ++ show (World [xs])
    show (World ((x:[]):ys)) = show x
        ++ "\n" ++ show (World ys)
    show (World ((x:xs):ys)) = show x
        ++ show (World [xs]) ++ "\n" ++ show (World ys)

data WorldTile =
      TilePlayer Player
    | TileEnemy Enemy
    | TileWall
    | TileEmpty

instance Show WorldTile where
    show (TilePlayer _) = "P"
    show (TileEnemy  _) = "E"
    show (TileWall    ) = "W"
    show (TileEmpty   ) = " "


{- /---------------------/
  /  Defaults
 /---------------------/ -}

default_player =
    Player
    { _player_stats   = default_entity_stats
    , _player_damage  = default_damage }

default_enemy =
    Enemy
    { _enemy_stats    = default_entity_stats
    , _enemy_damage   = default_damage }

default_entity_stats =
    EntityStats
    { _name          = "DefaultEntity"
    , _health        = 10
    , _level         = 1 }

default_damage =
    Damage
    { _amount        = 1
    , _damage_type   = TrueDamage }