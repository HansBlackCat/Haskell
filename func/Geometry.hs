module Geometry
(sphereVolume --, sphereArea, cubeVolume, cubeArea, cuboidArea, cuboidVolume
) where

sphereVolume radius = (4.0/3.0) * pi * (radius ^ 3)
