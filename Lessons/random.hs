import System.Random

rInt = random (mkStdGen 100) :: (Int, StdGen)
rBool = random (mkStdGen 100) :: (Bool, StdGen)
rFlaot = random (mkStdGen 100) :: (Float, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random r) => g -> [r]
randoms' g = let (value, newGen) = random g in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random r, Integral n) => n -> g -> ([r], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (v,newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (v:restOfList,finalGen)