module Spheres
  where
import           Utils.Circumsphere

n' :: Int
n' = 6

n :: Double
n = fromIntegral n'

halfside :: Double
halfside = sin (pi/n)

xI :: Double
xI = (1+halfside) / sin(pi/2-2*pi/n)

xS :: Double
xS = (1+halfside) * cos(2*pi/n)

yS :: Double
yS = (1+halfside) * sin(2*pi/n)

k :: Double
k = (xS-xI)*(xS-xI) + yS*yS

inversion :: Point -> Point
inversion (x,y,z) = (x',y',z')
  where
  sqnormIM = (x-xI)*(x-xI) + y*y + z*z
  c = k/sqnormIM
  x' = xI - c*(x-xI)
  y' = -c*y
  z' = -c*z

point :: Double -> Double -> Double -> Point -> Point
point theta phi r (cx,cy,cz) = (x,y,z)
  where
  x = r * cos theta * cos phi + cx
  y = r * sin theta * cos phi + cy
  z = r * sin phi + cz

sphere :: Int -> Int -> (Point, Double)
sphere frame i = ((cx,cy,0), radius)
  where
  shift = pi/90
  beta = fromIntegral i * 2*pi/n + fromIntegral frame * shift
  pt = (cos beta, sin beta ,0)
  p1 = inversion $ point 0 0 halfside pt
  p2 = inversion $ point (pi/2) 0 halfside pt
  p3 = inversion $ point pi 0 halfside pt
  p4 = inversion $ point 0 (pi/2) halfside pt
  ((cx,cy,_), radius) = circumsphere p1 p2 p3 p4

spheres :: Int -> [(Point,Double)]
spheres frame = map (sphere frame) [1..n']
