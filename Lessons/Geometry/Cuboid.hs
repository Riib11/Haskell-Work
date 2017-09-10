module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume w h d = w * h * d

area :: Float -> Float -> Float -> Float
area w h d = 2*((w*h)+(h*d)+(d*w))