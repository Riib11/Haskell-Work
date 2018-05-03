module LenseUtility
( createAccessors
) where

import Control.Lens

createAccessors :: Functor f =>
    ((b -> f b) -> a -> f a)    -- field
    -> a
    -> (a -> b, b -> a -> a)    -- getter, setter
createAccessors = _