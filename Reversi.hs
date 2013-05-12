import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Array
import Data.Array.IO
import Data.IORef

bufSize :: GLsizei
bufSize = 512

--global variables
black = Color3 (0.0::GLfloat) 0.0 0.0
white = Color3 (1.0::GLfloat) 1.0 1.0
green = Color3 (0.0::GLfloat) 0.619 0.043

data Player = Player1 | Player2 | EmptyCell
  deriving (Show, Eq)
not Player1 = Player2
not Player2 = Player1

 
horizontal old new = ((div old 8) == (div new 8) && boundary new)
up old new = ((div old 8)+1 == (div new 8) && boundary new)
down old new = ((div old 8)-1 == (div new 8) && boundary new)
boundary new = (new >= 0) && (new < 64)

updateArray _ [] _ = return ()
updateArray arr (x:xs) currentPlayer = do
  writeArray arr x currentPlayer
  updateArray arr xs currentPlayer
  
possiblyFoundPath arr xs currentPlayer playerSuccess = do
  if ((length xs) > 1)
    then do
      cp <- readIORef currentPlayer
      updateArray arr xs cp
      writeIORef playerSuccess True
    else return ()

goDirection current old arr diff func xs currentPlayer playerSuccess = do
  if (func old current)
    then do
      val <- readArray arr current
      cp <- readIORef currentPlayer
      putStrLn(show val ++ " " ++ show cp)
      if (cp == val)
        then do
          putStrLn("Path possibly found " ++ show (length xs))
          possiblyFoundPath arr (xs) currentPlayer playerSuccess
        else if (val == EmptyCell)
          then return ()
          else goDirection (current+diff) current arr diff func (current:xs) currentPlayer playerSuccess
    else return()
    
directionDispatcher _ _ _ _ [] = return ()
directionDispatcher index arr currentPlayer playerSuccess ((diff, func):pairs) = do
  goDirection (index+diff) index arr diff func [index] currentPlayer playerSuccess
  directionDispatcher index arr currentPlayer playerSuccess pairs

checkGameFinished _ 64 player1Count = return (True, player1Count)
checkGameFinished arr i player1Count = do
  val <- readArray arr i
  return False
  case val of
    EmptyCell -> return (False, player1Count)
    Player1 -> checkGameFinished arr (i+1) (player1Count+1)
    Player2 -> checkGameFinished arr (i+1) player1Count
    
    
    --_ -> checkGameFinished arr (i+1) player1Count
  -- if (val == EmptyCell)
    -- then return False
    -- else checkGameFinished arr (i+1) player1Count

gameFinished player1Count = do
  if (player1Count == 32)
    then putStrLn "Tie Game"
    else case (player1Count >32) of
      True -> putStrLn "Player 1 Wins" 
      False -> putStrLn "Player 2 Wins"

    
testfunc ((Name x):xs) arr currentPlayer = do
  playerSuccess <- newIORef False
  let index = (fromIntegral(x) :: Int)
      pairs = [(1, horizontal), (-7, down), (-8, down), (-9, down), (-1, horizontal), (7, up), (8, up), (9, up)]
  
  val <- readArray arr index
  if (val == EmptyCell)    
    then directionDispatcher index arr currentPlayer playerSuccess pairs
    else return ()
  
  bool <- readIORef playerSuccess
  cp <- readIORef currentPlayer
  if (bool)
    then if (cp == Player1)
        then writeIORef currentPlayer Player2
        else writeIORef currentPlayer Player1
    else return ()
    
  (finished, player1Count) <- checkGameFinished arr 0 0
  if (finished)
    then gameFinished player1Count
    else return ()
 
--processHits prints the hit records.
processHits _ Nothing _ = putStrLn "selection buffer overflow"
processHits arr (Just hitRecords) currentPlayer = testfunc (map (\(HitRecord z1 z2 (x:xs)) -> (x)) hitRecords) arr currentPlayer


drawCircle _ _ 360 = return ()
drawCircle x y i = do
  let x1 = 0.5*cos(i*pi/180) + x + 0.5
      y1 = 0.5*sin(i*pi/180) + y + 0.5
  vertex $ (Vertex3 ((x1)::GLfloat) y1 0)
  drawCircle x y (i+1)

--drawPieces :: (Integral t, Num a, Ix a, MArray a1 t IO) => a1 a t -> a -> IO ()
drawPieces arr i = do
  b <- readArray arr i
  if (b /= EmptyCell)
    then do
      if (b == Player1) 
        then color $ black
        else color $ white
      let j = (fromIntegral(i) :: Int)
          x = (fromIntegral(mod j 8)::GLfloat)
          y = (fromIntegral(div j 8)::GLfloat)
      renderPrimitive Polygon $ drawCircle x y 0
    else
      return ()
  if (i < 63)
    then drawPieces arr (i+1)
    else return ()
  
--renderBoard :: GLfloat -> GLfloat -> PrimitiveMode -> GLuint -> IO ()
renderBoard x y prim name = do
    loadName (Name name)
    renderPrimitive prim $ do
      vertex $ (Vertex3 ((x)::GLfloat) y 0)
      vertex $ (Vertex3 ((x+1)::GLfloat) y 0)
      vertex $ (Vertex3 ((x+1)::GLfloat) (y+1) 0)
      vertex $ (Vertex3 ((x)::GLfloat) (y+1) 0)
    if (x == 7 && y==(7))
      then return ()
      else if (x==7)
        then renderBoard 0 (y+1) prim (name+1)
        else renderBoard (x+1) y prim (name+1)
    

--pickRects :: (Num e, MArray a e IO) => a Int e -> Key -> KeyState -> t -> Position -> IO ()
pickRects arr currentPlayer (MouseButton LeftButton) Down _ (Position x y) = do
  vp@(_, (Size _ height)) <- get viewport
  (_, maybeHitRecords) <- getHitRecords bufSize $
    withName (Name 0) $ do
      matrixMode $= Projection
      preservingMatrix $ do
          loadIdentity
          -- create 1x1 pixel picking region near cursor location
          pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (1, 1) vp
          ortho 0 8 0 8 (-0.5) 2.5
          renderBoard 0 0 Quads 0
      flush
  cp <- readIORef currentPlayer
  -- if (cp == Player1)
    -- then putStrLn "Player1"
    -- else putStrLn "Player2"
  processHits arr maybeHitRecords currentPlayer
  postRedisplay Nothing
pickRects _ _ (Char '\27') Down _ _ = exitWith ExitSuccess
pickRects _ _ (Char '\113') Down _ _ = exitWith ExitSuccess
pickRects _ _  _            _    _ _ = return ()


--display :: IO ()
display arr = do
  clear [ ColorBuffer, DepthBuffer ]
  color $ black
  renderBoard 0 0 LineLoop 0
  drawPieces arr (0 :: Int)
  flush

--reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho 0 8 0 8 (-0.5) 2.5
   matrixMode $= Modelview 0
   loadIdentity


--myInit :: IO ()
myInit = do
   clearColor $= Color4 (0.0::GLfloat) 0.619 0.043 0
   depthFunc $= Just Less
   shadeModel $= Flat
   depthRange $= (0, 1)   -- The default z mapping

--TODO: game needs to recognize if one player loses all chips
--main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  arr <- newArray (0,63) EmptyCell :: IO (IOArray Int Player)
  currentPlayer <- newIORef Player1
  writeArray arr 27 Player1
  writeArray arr 28 Player2
  writeArray arr 35 Player2
  writeArray arr 36 Player1
  
  initialWindowSize $= Size 400 400
  createWindow "Reversi"
  myInit
  reshapeCallback $= Just reshape
  displayCallback $= (display arr)
  keyboardMouseCallback $= Just (pickRects arr currentPlayer)
  mainLoop
