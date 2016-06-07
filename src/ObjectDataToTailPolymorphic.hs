-- Map object data to tail-polymorphic record types
module Main where

import Control.Monad
import Data.Monoid

data Shape w = Shape
  { getX      :: Int
  , getY      :: Int
  , shapeTail :: w
  }

shape :: Int -> Int -> w -> Shape w
shape x y w = Shape
  { getX      = x
  , getY      = y
  , shapeTail = w
  }

setX :: Int -> Shape w -> Shape w
setX i s = s { getX = i }

setY :: Int -> Shape w -> Shape w
setY i s = s { getY = i }

moveTo :: Int -> Int -> Shape w -> Shape w
moveTo x y = setY y . setX x

rMoveTo :: Int -> Int -> Shape w -> Shape w
rMoveTo deltax deltay s = moveTo x y s
  where
    x = getX s + deltax
    y = getY s + deltay

class Draw w where
  draw :: Shape w -> IO ()

data RectangleDelta w = RectangleDelta
  { getWidth      :: Int
  , getHeight     :: Int
  , rectangleTail :: w
  }

type Rectangle w = Shape (RectangleDelta w)

rectangle x y w h = shape x y $ RectangleDelta
  { getWidth      = w
  , getHeight     = h
  , rectangleTail = ()
  }

setHeight :: Int -> Rectangle w -> Rectangle w
setHeight i s =
  s { shapeTail = (shapeTail s) { getHeight = i } }

setWidth :: Int -> Rectangle w -> Rectangle w
setWidth i s = s { shapeTail = (shapeTail s) { getWidth = i } }

instance Draw (RectangleDelta w) where
  draw s = putStrLn $ mconcat
    [ "Drawing a Rectangle at:("
    , show $ getX s
    , ","
    , show $ getY s
    , "), width " <> show (getWidth $ shapeTail s)
    , ", heigth " <> show (getHeight $ shapeTail s)
    ]

data CircleDelta w = CircleDelta
  { getRadius  :: Int
  , circleTail :: w
  }

type Circle w = Shape (CircleDelta w)

circle x y r = shape x y $ CircleDelta
  { getRadius  = r
  , circleTail = ()
  }

setRadius :: Int -> Circle w -> Circle w
setRadius i s =
  s { shapeTail = (shapeTail s) { getRadius = i } }

instance Draw (CircleDelta w) where
  draw s = putStrLn $ mconcat
    [ "Drawing a Circle at:("
    , show $ getX s
    , ","
    , show $ getY s
    , "), radius "
    , show (getRadius $ shapeTail s)
    ]

tagShape :: (w -> w') -> Shape w -> Shape w'
tagShape f s = s { shapeTail = f (shapeTail s) }

eitherShape :: (Shape w  -> t)
            -> (Shape w' -> t)
            -> Shape (Either w w')
            -> t
eitherShape f g s = case shapeTail s of
  Left  s' -> f $ s { shapeTail = s' }
  Right s' -> g $ s { shapeTail = s' }

instance (Draw a, Draw b) => Draw (Either a b) where
  draw = eitherShape draw draw

main :: IO ()
main = do
  let scribble =
        [ tagShape Left $ rectangle 10 20 5 6
        , tagShape Right $ circle 15 25 8
        ]
  forM_ scribble $ \x -> do
    draw x
    draw $ rMoveTo 100 100 x
