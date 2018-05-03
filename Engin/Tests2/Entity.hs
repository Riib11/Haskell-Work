module Entity
( Entity (Entity), setName, setProperties
, getEntityProperty, setEntityProperty
, getEntityProperties, setEntityProperties
, entity_blank
, Properties
, Property (PropString, PropInt, PropBool)
, getProperty, setProperty, deleteProperty
) where

import Data.Map

-- Entity

data Entity = Entity
    { name :: String
    , properties :: Properties
    } deriving (Show)

setName :: String -> Entity -> Entity
setName n' (Entity n ps) = Entity n' ps

setProperties :: Properties -> Entity -> Entity
setProperties ps' (Entity n ps) = Entity n ps'

-- EntityProperty

getEntityProperty :: String -> Entity -> Maybe Property
getEntityProperty k (Entity n ps) = getProperty k ps

setEntityProperty :: String -> Property -> Entity -> Entity
setEntityProperty k v (Entity n ps) = Entity n (setProperty k v ps)

-- EntityProperties

getEntityProperties :: [String] -> Entity -> [Maybe Property]
getEntityProperties [] ent = []
getEntityProperties (x:xs) ent =
    getEntityProperty x ent : getEntityProperties xs ent

setEntityProperties :: [String] -> [Property] -> Entity -> Entity
setEntityProperties [] [] ent = ent
setEntityProperties (x:xs) (y:ys) ent =
    setEntityProperty x y $ setEntityProperties xs ys ent

-- Defaults

entity_blank :: Entity
entity_blank = Entity n ps
    where
        n  = "empty"
        ps = empty :: Properties

-- Properties

type Properties = Map String Property

data Property = PropString String
              | PropInt Int
              | PropBool Bool
                deriving (Eq,Show)

setProperty :: String -> Property -> Properties -> Properties
setProperty k v' ps = alter set k ps
    where
        set Nothing  = Nothing
        set (Just v) = Just v'

deleteProperty :: String -> Properties -> Properties
deleteProperty = alter (\_ -> Nothing)

getProperty :: String -> Properties -> Maybe Property
getProperty k = Data.Map.lookup k


-- Test

entity_test = Entity
    { name = "Henry"
    , properties = fromList
        [ ("class",  PropString  "developer")
        , ("level",  PropInt     1)
        , ("active", PropBool    True) ] }