module PGM
( writeGrid
) where

import Grid

max_value = 2 :: Int

pgmHeader :: String -> String
pgmHeader name = "P2\n# " ++ name ++ ".pgm\n"

gridmatrixToPGMRepresentation :: [[GridTile]] -> String
gridmatrixToPGMRepresentation [[]] = ""
gridmatrixToPGMRepresentation ([]:cs) = gridmatrixToPGMRepresentation cs
gridmatrixToPGMRepresentation ((r:rs):cs) =
    (show $ gridtileColor r) ++ " "
    ++ gridmatrixToPGMRepresentation (rs:cs)

gridToPGM :: String -> Grid -> String
gridToPGM name (Grid size grid) =
    pgmHeader name
    ++ show size ++ " " ++ show size ++ "\n"
    ++ show max_value ++ "\n\n"
    ++ gridmatrixToPGMRepresentation grid
    ++ "\n"

writePGM :: String -> String -> IO ()
writePGM name = writeFile ("grids/" ++ name ++ ".pgm")

writeGrid :: String -> Grid -> IO ()
writeGrid name grid = writePGM name $ gridToPGM name grid