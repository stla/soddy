module HierarchicalSteiner3D2
  where
import           Linear             hiding (point, vector)
import           Utils.Circumsphere

fromV3 :: V3 Double -> Point
fromV3 (V3 a b c) = (a, b, c)

toV3 :: Point -> V3 Double
toV3 (a, b, c) = V3 a b c

inversion :: Double -> V3 Double -> Double -> V3 Double -> Point
inversion phi point radius center =
  fromV3 $ omega ^+^ center - k / quadrance vector *^ vector
  where
  invphi = 1/phi
  omega = V3 (invphi * radius) 0 0
  k = radius*radius * (invphi*invphi-1)
  vector = point ^-^ omega ^-^ center

oneSphere :: Int -> Double -> V3 Double -> Double -> Double -> (Point,Double)
oneSphere n phi center radius beta = (c, r)
  where
  sine = sin(pi / fromIntegral n)
  coef = 1 / (1+sine)
  cradius = coef * radius
  pt = center ^+^ V3 (cradius * cos beta) (cradius * sin beta) 0
  sRadius = cradius * sine
  p1 = inversion phi (pt ^+^ V3 sRadius 0 0) radius center
  p2 = inversion phi (pt ^+^ V3 0 sRadius 0) radius center
  p3 = inversion phi (pt ^+^ V3 (-sRadius) 0 0) radius center
  p4 = inversion phi (pt ^+^ V3 0 0 sRadius) radius center
  ((cx,cy,_),r) = circumsphere' p1 p2 p3 p4
  c = (cx - 2/phi*radius, cy, 0)

chain :: Int -> Double -> Int -> Bool -> (Point,Double) -> [(Point,Double)]
chain n phi frame clockwise (center,radius) =
  map (oneSphere n phi center' radius) angles
  where
  center' = toV3 center
  frame' = fromIntegral frame :: Double
  shift = frame' * (if clockwise then -pi/90 else pi/90)
  angles = map (+shift) [2 * pi * frac i | i <- [0..n-1]]
    where
    frac p = fromIntegral p / fromIntegral n

chains :: Int -> Double -> Int -> Int -> Bool -> [(Point,Double)]
chains n phi depth frame clockwise
  | depth==0 = [((0, 0, 0), 1)]
  | otherwise = let nclockwise = not clockwise in
                concatMap (chain n phi frame nclockwise)
                          (chains n phi (depth-1) frame nclockwise)
