module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

sphereVolume :: Float f => f -> f
sphereVolume radius = (4.0/3.0) * pi * (radius ^ 3)

sphereArea :: Float f => f -> f
sphereArea radius = 4 * pi * (radius ^ 3)

cubeVolume :: Float f => f -> f
cubeVolume side = side ^ 3

cubeArea :: Float f => f -> f
cubeArea side = 6 * (side ^ 2)

cuboidVolume :: Float f => f -> f -> f -> f
cuboidVolume w h d = w * h * d

cuboidArea :: Float f => f -> f -> f -> f
cuboidArea w h d = 2*((w*h)+(h*d)+(d*w))