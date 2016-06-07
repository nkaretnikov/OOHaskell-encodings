-- Map subtype hierarchy to an algebraic datatype
module Main where

import Control.Monad
import Data.Monoid

-- | Each kind of shape is a constructor.
data Shape
  = Rectangle
      { getX :: Int
      , getY :: Int
      , getWidth :: Int
      , getHeight :: Int
      }
  | Circle
      { getX :: Int
      , getY :: Int
      , getRadius :: Int
      }

-- * Setters
setX :: Int -> Shape -> Shape
setX i s = s { getX = i }

setY :: Int -> Shape -> Shape
setY i s = s { getY = i }

setWidth :: Int -> Shape -> Shape
setWidth i s = s { getWidth = i }

-- * Functions
moveTo :: Int -> Int -> Shape -> Shape
moveTo x y = setY y . setX x

rMoveTo :: Int -> Int -> Shape -> Shape
rMoveTo deltax deltay s = moveTo x y s
  where
    x = getX s + deltax
    y = getY s + deltay

draw :: Shape -> IO ()
draw s@Rectangle {} = putStrLn $ mconcat
  [ "Drawing a Rectangle at:("
  , show $ getX s
  , ","
  , show $ getY s
  , "), width " <> show (getWidth s)
  , ", heigth " <> show (getHeight s)
  ]
draw s@Circle {} = putStrLn $ mconcat
  [ "Drawing a Circle at:("
  , show $ getX s
  , ","
  , show $ getY s
  , "), radius "
  , show (getRadius s)
  ]

main :: IO ()
main = do
  let scribble =
        [ Rectangle 10 20 5 6
        , Circle 15 25 8
        ]
  forM_ scribble $ \x -> do
    draw x
    draw $ rMoveTo 100 100 x
