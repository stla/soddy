module SoddyHierarchical2
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           HierarchicalHexlet2
import           System.Directory                  (doesDirectoryExist)
import           Text.Printf


white,black,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1

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
  spheres <- get (contextSpheres context)
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
  mapM_ (\(center,radius) -> preservingMatrix $ do
                  translate (toVector3 center)
                  materialDiffuse Front $= red
                  renderObject Solid $ Sphere' radius 60 60)
        spheres
  swapBuffers
  where
    toVector3 (x,y,z) = Vector3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Int -- depth
         -> IORef Int -- "frame"
         -> IORef Bool -- animation
         -> IORef [((Double,Double,Double),Double)]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom depth frame anim spheres c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'h' -> do
      depth $~! (+1)
      depth' <- get depth
      frame' <- get frame
      writeIORef spheres (hexlets depth' frame' True)
    'n' -> do
      depth $~! (\n -> if n>0 then n-1 else n)
      depth' <- get depth
      frame' <- get frame
      writeIORef spheres (hexlets depth' frame' True)
    'g' -> do
      frame $~! (+1)
      frame' <- get frame
      depth' <- get depth
      writeIORef spheres (hexlets depth' frame' True)
    'b' -> do
      frame $~! subtract 1
      frame' <- get frame
      depth' <- get depth
      writeIORef spheres (hexlets depth' frame' True)
    'q' -> leaveMainLoop
    'a' -> writeIORef anim True
    's' -> writeIORef anim False
    _   -> return ()
  postRedisplay Nothing

idle :: IORef Bool -> IORef Int -> IORef Int
     -> IORef [((Double,Double,Double),Double)] -> IdleCallback
idle anim depth frame spheres = do
  anim' <- get anim
  when anim' $ do
    frame $~! (+1)
    frame' <- get frame
    depth' <- get depth
    writeIORef spheres (hexlets depth' frame' True)
    ppmExists <- doesDirectoryExist "./ppm"
    when (ppmExists && frame' <= 30) $ do
      let ppm = printf "ppm/pic%04d.ppm" frame'
      (>>=) capturePPM (B.writeFile ppm)
    postRedisplay Nothing
  return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Soddy's hierarchical hexlet"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-500) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  let depth = 1
      frame = 0
      spheres = hexlets depth frame True
  depth' <- newIORef depth
  frame' <- newIORef frame
  spheres' <- newIORef spheres
  anim <- newIORef False
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextSpheres = spheres'}
                             zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $=
    Just (keyboard rot1 rot2 rot3 zoom depth' frame' anim spheres')
  idleCallback $= Just (idle anim depth' frame' spheres')
  putStrLn "*** Soddy's hierarchical hexlet ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease depth: h, n\n\
        \    Rotate hexlets: g, b\n\
        \    Animation : a, s\n\
        \"
  mainLoop
