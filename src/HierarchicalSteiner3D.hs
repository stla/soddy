module HierarchicalSteiner3D
  where
import           Linear             hiding (point, vector)
import           Utils.Circumsphere

fromV3 :: V3 Double -> Point
fromV3 (V3 a b c) = (a, b, c)

toV3 :: Point -> V3 Double
toV3 (a, b, c) = V3 a b c

inversion :: Int -> V3 Double -> Double -> V3 Double -> Point
inversion n point radius center =
  fromV3 $ omega ^+^ center - k / quadrance vector *^ vector
  where
  n' = fromIntegral n :: Double
  a = if n==3 || n==4 then pi/n' else 2*pi/n'
  omega = V3 (radius / sin(pi/2-a)) 0 0
  s = V3 (radius * cos a) (radius * sin a) 0
  k = quadrance (s ^-^ omega)
  vector = point ^-^ omega ^-^ center

oneSphere :: Int -> V3 Double -> Double -> Double -> (Point,Double)
oneSphere n center radius beta = (c, r)
  where
  n' = fromIntegral n :: Double
  coef = 1 / (1+sin(pi/n'))
  cradius = coef * radius
  pt = center ^+^ V3 (cradius * cos beta) (cradius * sin beta) 0
  sRadius = coef * radius * sin(pi/n')
  p1 = inversion n (pt ^+^ V3 sRadius 0 0) radius center
  p2 = inversion n (pt ^+^ V3 0 sRadius 0) radius center
  p3 = inversion n (pt ^+^ V3 (-sRadius) 0 0) radius center
  p4 = inversion n (pt ^+^ V3 0 0 sRadius) radius center
  ((cx,cy,_),r) = circumsphere p1 p2 p3 p4
  a = if n==3 || n==4 then pi/n' else 2*pi/n'
  c = (cx - 2*radius/sin(pi/2-a), cy, 0)

chain :: Int -> Int -> Bool -> (Point,Double) -> [(Point,Double)]
chain n frame clockwise (center,radius) =
  map (oneSphere n center' radius) angles
  where
  center' = toV3 center
  frame' = fromIntegral frame :: Double
  shift = frame' * (if clockwise then -pi/90 else pi/90)
  angles = map (+shift) [2 * pi * frac i | i <- [0..n-1]]
    where
    frac p = fromIntegral p / fromIntegral n

chains :: Int -> Int -> Int -> Bool -> [(Point,Double)]
chains n depth frame clockwise
  | depth==0 = [((0, 0, 0), 1)]
  | otherwise = let nclockwise = not clockwise in
                concatMap (chain n frame nclockwise)
                          (chains n (depth-1) frame nclockwise)
