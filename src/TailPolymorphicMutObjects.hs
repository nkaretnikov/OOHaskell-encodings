-- Mutable objects, again with tail polymorphism

module Main where

import Control.Monad
import Control.Monad.Fix
import Data.IORef

data Shape w = Shape
  { getX      :: IO Int
  , getY      :: IO Int
  , setX      :: Int -> IO ()
  , setY      :: Int -> IO ()
  , moveTo    :: Int -> Int -> IO ()
  , rMoveTo   :: Int -> Int -> IO ()
  , draw      :: IO ()
  , shapeTail :: w
  }

returnIO :: a -> IO a
returnIO = return

shape x y tail self = do
  xRef <- newIORef x
  yRef <- newIORef y
  tail' <- tail
  returnIO Shape
    { getX      = readIORef xRef
    , getY      = readIORef yRef
    , setX      = \x' -> writeIORef xRef x'
    , setY      = \y' -> writeIORef yRef y'
    , moveTo    = \x' y' -> do
                     setX self x'
                     setY self y'
    , rMoveTo   = \deltax deltay -> do
                     x <- getX self
                     y <- getY self
                     moveTo self (x+deltax) (y+deltay)
    , draw      = putStrLn "Nothing to draw"
    , shapeTail = tail' self
    }

type Rectangle w = Shape (RectangleDelta w)

data RectangleDelta w = RectangleDelta
  { getWidth'     :: IO Int
  , getHeight'    :: IO Int
  , setWidth'     :: Int -> IO ()
  , setHeight'    :: Int -> IO ()
  , rectangleTail :: w
  }

getWidth  = getWidth'  . shapeTail
getHeight = getHeight' . shapeTail
setWidth  = setWidth'  . shapeTail
setHeight = setHeight' . shapeTail

ls = putStr
(<<) = (>>)
sp = (putStr . show =<<)

rectangle x y w h self = do
  super <- shape x y shapeTail self
  returnIO super { draw = drawRectangle self }
  where
    drawRectangle self =
      putStr "Drawing a Rectangle at:(" <<
      sp (getX self) << ls "," << sp (getY self) <<
      ls "), width " << sp (getWidth self) <<
      ls ", height " << sp (getHeight self) <<
      ls "\n"
    shapeTail = do
      wRef <- newIORef w
      hRef <- newIORef h
      returnIO $ \self ->
        RectangleDelta
          { getWidth'     = readIORef wRef
          , getHeight'    = readIORef hRef
          , setWidth'     = \w' -> writeIORef wRef w'
          , setHeight'    = \h' -> writeIORef hRef h'
          , rectangleTail = ()
          }

type Circle w = Shape (CircleDelta w)

data CircleDelta w = CircleDelta
  { getRadius' :: IO Int
  , setRadius' :: Int -> IO ()
  , circleTail :: w
  }

getRadius = getRadius' . shapeTail
setRadius = setRadius' . shapeTail

circle x y r self = do
  super <- shape x y shapeTail self
  returnIO super { draw = drawCircle self }
  where
    drawCircle self =
      putStr "Drawing a Circle at:(" <<
      sp (getX self) << ls "," << sp (getY self) <<
      ls "), radius " << sp (getRadius self) <<
      ls "\n"
    shapeTail = do
      rRef <- newIORef r
      returnIO $ \self ->
        CircleDelta
          { getRadius' = readIORef rRef
          , setRadius' = \r' -> writeIORef rRef r'
          , circleTail = ()
          }

narrowToShape :: Shape w -> Shape ()
narrowToShape s = s { shapeTail = () }

main = do
  s1 <- mfix $ rectangle 10 20 5 6
  s2 <- mfix $ circle 15 25 8
  let scribble =
        [ narrowToShape s1
        , narrowToShape s2
        ]
  forM_ scribble $ \x -> do
    draw x
    rMoveTo x 100 100
    draw x
