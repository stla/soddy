module Utils.Circumsphere
  where
import           Data.List (zipWith3)

type Point = (Double, Double, Double)

det3x3 :: Point -> Point -> Point -> Double
det3x3 (x1,x2,x3) (y1,y2,y3) (z1,z2,z3) =
  x1*y2*z3 + x2*y3*z1 + x3*y1*z2 - (x3*y2*z1) - (x2*y1*z3) - (x1*y3*z2)

circumsphere :: Point -> Point -> Point -> Point -> (Point, Double)
circumsphere (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) (x4,y4,z4) =
  ((cx, cy, cz), radius)
  where
    det4x4 [u1,u2,u3,u4] [v1,v2,v3,v4] [w1,w2,w3,w4] =
      - det3x3 (u2,u3,u4) (v2,v3,v4) (w2,w3,w4) +
      det3x3 (u1,u3,u4) (v1,v3,v4) (w1,w3,w4) -
      det3x3 (u1,u2,u4) (v1,v2,v4) (w1,w2,w4) +
      det3x3 (u1,u2,u3) (v1,v2,v3) (w1,w2,w3)
    det4x4 _ _ _ = undefined
    x = [x1,x2,x3,x4]
    y = [y1,y2,y3,y4]
    z = [z1,z2,z3,z4]
    a = det4x4 x y z
    ssq = zipWith3 (\u v w -> u*u+v*v+w*w) x y z
    dx = det4x4 ssq y z
    dy = det4x4 ssq x z
    dz = det4x4 ssq x y
    cx = dx/2/a
    cy = - dy/2/a
    cz = dz/2/a
    radius = sqrt ((cx-x1)*(cx-x1)+(cy-y1)*(cy-y1)+(cz-z1)*(cz-z1))
