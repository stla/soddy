module Soddy5
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Spheres
import           Text.Printf

white,black,c1,c2,c3,c4,c5,c6,twhite :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
c1         = Color4 0.12 0.47 0.71    1
c2         = Color4    1  0.5 0.05    1
c3         = Color4 0.17 0.63 0.17    1
c4         = Color4 0.84 0.15 0.16    1
c5         = Color4 0.58  0.4 0.74    1
c6         = Color4 0.55 0.34 0.29    1
twhite     = Color4    1    1    1  0.1

data Context = Context
    {
      contextRot1    :: IORef GLfloat
    , contextRot2    :: IORef GLfloat
    , contextRot3    :: IORef GLfloat
    , contextSpheres :: IORef [((Double,Double,Double),Double)]
    }

display :: Context -> IORef Double -> DisplayCallback
display context zoom = do
  clear [ColorBuffer, DepthBuffer]
  nspheres <- get (contextSpheres context)
  zoom' <- get zoom
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  loadIdentity
  (_, size) <- get viewport
  resize zoom' size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (\((center,radius),col) -> preservingMatrix $ do
                  translate (toVector3 center)
                  materialDiffuse Front $= col
                  renderObject Solid $ Sphere' radius 60 60)
        (zip nspheres [c1,c2,c3,c4,c5,c6])
  preservingMatrix $ do
    translate (Vector3 (2*xI) 0 0)
    materialDiffuse Front $= twhite
    renderObject Solid $ Sphere' (1+halfside) 60 60
  swapBuffers
  where
    toVector3 (x,y,z) = Vector3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 6 0 (-5+zoom)) (Vertex3 6 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Int -> IORef [((Double,Double,Double),Double)] -> IdleCallback
idle frame nspheres = do
  frame $~! (+1)
  frame' <- get frame
  when (frame' <= 180) $ do
    let nspheres' = spheres frame'
    writeIORef nspheres nspheres'
    let ppm = printf "ppm/pic%04d.ppm" frame'
    (>>=) capturePPM (B.writeFile ppm)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Soddy's hexlet"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 60 0 (-500) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  let nspheres = spheres 1
  nspheres' <- newIORef nspheres
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextSpheres = nspheres'}
                             zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  frame <- newIORef 0
  idleCallback $= Just (idle frame nspheres')
  mainLoop
