module HierarchicalHexlet2
  where
import           Linear             hiding (point, vector)
import           Utils.Circumsphere

fromV3 :: V3 Double -> Point
fromV3 (V3 a b c) = (a, b, c)

toV3 :: Point -> V3 Double
toV3 (a, b, c) = V3 a b c

inversion :: V3 Double -> Double -> V3 Double -> Point
inversion point radius center =
  fromV3 $ omega ^+^ center - k / quadrance vector *^ vector
  where
  omega = V3 (2*radius) 0 0
  s = V3 (radius * 0.5) (radius * sqrt 3 / 2) 0
  k = quadrance (s ^-^ omega)
  vector = point ^-^ omega ^-^ center

pointOnSphere :: Double -> Double -> Double -> V3 Double -> V3 Double
pointOnSphere theta phi r center = r *^ point ^+^ center
  where
  point = V3 (cos theta * cos phi) (sin theta * cos phi) (sin phi)

oneSphere :: V3 Double -> Double -> Double -> (Point,Double)
oneSphere center radius beta = (c, r)
  where
  pt = center ^+^ V3 (radius * cos beta * 2 / 3) (radius * sin beta * 2 / 3) 0
  thRadius = radius / 3
  p1 = inversion (pointOnSphere 0 0 thRadius pt) radius center
  p2 = inversion (pointOnSphere (pi/2) 0 thRadius pt) radius center
  p3 = inversion (pointOnSphere pi 0 thRadius pt) radius center
  p4 = inversion (pointOnSphere 0 (pi/2) thRadius pt) radius center
  ((cx,cy,_),r) = circumsphere p1 p2 p3 p4
  c = (cx - 4*radius, cy, 0)

hexlet :: Int -> Bool -> (Point,Double) -> [(Point,Double)]
hexlet frame clockwise (center,radius) = map (oneSphere center' radius) angles
  where
  center' = toV3 center
  frame' = fromIntegral frame :: Double
  shift = frame' * (if clockwise then -pi/90 else pi/90)
  angles = map (+shift) [0.0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3]

-- hexlets :: Int -> [(Point,Double)]
-- hexlets n | n==1 = hexlet ((0, 0, 0), 1)
--           | otherwise = concatMap hexlet (hexlets (n-1))

hexlets :: Int -> Int -> Bool -> [(Point,Double)]
hexlets n frame clockwise
  | n==0 = [((0, 0, 0), 1)]
  | otherwise = let nclockwise = not clockwise in
                concatMap (hexlet frame nclockwise)
                          (hexlets (n-1) frame nclockwise)
