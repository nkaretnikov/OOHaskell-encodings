-- Functional objects, again with tail polymorphism
module Main where

import Control.Monad
import Data.Monoid

data Shape w = Shape
  { getX      :: Int
  , getY      :: Int
  , setX      :: Int -> Shape w
  , setY      :: Int -> Shape w
  , moveTo    :: Int -> Int -> Shape w
  , rMoveTo   :: Int -> Int -> Shape w
  , draw      :: IO ()
  , shapeTail :: w
  }

shape :: Int -> Int -> (Int -> Int -> IO ()) -> w -> Shape w
shape x y d t = Shape
  { getX      = x
  , getY      = y
  , setX      = \x'    -> shape x' y  d t
  , setY      = \y'    -> shape x  y' d t
  , moveTo    = \x' y' -> shape x' y' d t
  , rMoveTo   = \deltax deltay
                       -> shape (x+deltax) (y+deltay) d t
  , draw      = d x y
  , shapeTail = t
  }

type Rectangle w = Shape (RectangleDelta w)

data RectangleDelta w = RectangleDelta
  { getWidth'     :: Int
  , getHeight'    :: Int
  , setWidth'     :: Int -> Rectangle w
  , setHeight'    :: Int -> Rectangle w
  , rectangleTail :: w
  }

getWidth  = getWidth'  . shapeTail
getHeight = getHeight' . shapeTail
setWidth  = setWidth'  . shapeTail
setHeight = setHeight' . shapeTail

rectangle :: Int -> Int -> Int -> Int -> Shape (RectangleDelta ())
rectangle x y w h = shape x y drawRectangle shapeTail
  where
    drawRectangle x y = putStrLn $ mconcat
      [ "Drawing a Rectangle at:("
      , show x
      , ","
      , show y
      , "), width " <> show w
      , ", heigth " <> show h
      ]
    shapeTail = RectangleDelta
      { getWidth'     = w
      , getHeight'    = h
      , setWidth'     = \w' -> rectangle x y w' h
      , setHeight'    = \h' -> rectangle x y w  h'
      , rectangleTail = ()
      }

type Circle w = Shape (CircleDelta w)

data CircleDelta w = CircleDelta
  { getRadius' :: Int
  , setRadius' :: Int -> Circle w
  , circleTail :: w
  }

getRadius = getRadius' . shapeTail
setRadius = setRadius' . shapeTail

circle :: Int -> Int -> Int -> Shape (CircleDelta ())
circle x y r = shape x y drawCircle shapeTail
  where
    drawCircle x y = putStrLn $ mconcat
      [ "Drawing a Circle at:("
      , show x
      , ","
      , show y
      , "), radius "
      , show r
      ]
    shapeTail = CircleDelta
      { getRadius' = r
      , setRadius' = \r' -> circle x y r'
      , circleTail = ()
      }

narrowToShape :: Shape w -> Shape ()
narrowToShape s = s
  { setX      = narrowToShape . setX s
  , setY      = narrowToShape . setY s
  , moveTo    = \z -> narrowToShape . moveTo  s z
  , rMoveTo   = \z -> narrowToShape . rMoveTo s z
  , shapeTail = ()
  }

main :: IO ()
main = do
  let scribble =
        [ narrowToShape $ rectangle 10 20 5 6
        , narrowToShape $ circle 15 25 8
        ]
  forM_ scribble $ \x -> do
    draw x
    draw $ rMoveTo x 100 100
