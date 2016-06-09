-- Subtypes as composed record types with overloading
-- with existential quantification

{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad
import Data.Monoid

data OpaqueShape = forall x. Shape x => HideShape x

instance Shape OpaqueShape where
  readShape  f (HideShape x) = readShape f x
  writeShape f (HideShape x) = HideShape $ writeShape f x
  draw         (HideShape x) = draw x

data ShapeData = ShapeData
  { valX :: Int
  , valY :: Int
  }

shape x y = ShapeData
  { valX = x
  , valY = y
  }

class Shape s where
  getX       :: s -> Int
  setX       :: Int -> s -> s
  getY       :: s -> Int
  setY       :: Int -> s -> s
  moveTo     :: Int -> Int -> s -> s
  rMoveTo    :: Int -> Int -> s -> s
  draw       :: s -> IO ()
  readShape  :: (ShapeData -> t)         -> s -> t
  writeShape :: (ShapeData -> ShapeData) -> s -> s

  getX       = readShape valX
  setX i     = writeShape $ \s -> s { valX = i }
  getY       = readShape valY
  setY i     = writeShape $ \s -> s { valY = i }
  moveTo x y = setY y . setX x
  rMoveTo deltax deltay s = moveTo x y s
    where
      x = getX s + deltax
      y = getY s + deltay

data RectangleData = RectangleData
  { rdValShape  :: ShapeData
  , rdValWidth  :: Int
  , rdValHeight :: Int
  }

rectangle x y w h = RectangleData
  { rdValShape  = shape x y
  , rdValWidth  = w
  , rdValHeight = h
  }

-- | A rectangle is a shape.
instance Shape RectangleData where
  readShape f = f . rdValShape
  writeShape f s = s { rdValShape = readShape f s }
  draw s = putStrLn $ mconcat
    [ "Drawing a Rectangle at:("
    , show (getX s) <> ","
    , show (getY s) <> "), width "
    , show (rGetWidth s) <> ", height "
    , show (rGetHeight s)
    ]

-- | OO subclassing coincides in Haskell typeclass subclassing.
class Shape s => Rectangle s where
  rGetWidth :: s -> Int
  rGetWidth = readRectangle rdValWidth

  rSetWidth :: Int -> s -> s
  rSetWidth i = writeRectangle $ \s -> s { rdValWidth = i }

  rGetHeight :: s -> Int
  rGetHeight = readRectangle rdValHeight

  rSetHeight :: Int -> s -> s
  rSetHeight i = writeRectangle $ \s -> s { rdValHeight = i }

  readRectangle  :: (RectangleData -> t)             -> s -> t
  writeRectangle :: (RectangleData -> RectangleData) -> s -> s

-- | A rectangle is nothing but a rectangle.
instance Rectangle RectangleData where
  readRectangle  = id
  writeRectangle = id

data CircleData = CircleData
  { cdValShape  :: ShapeData
  , cdValRadius :: Int
  }

circle x y r = CircleData
  { cdValShape = shape x y
  , cdValRadius = r
  }

instance Shape CircleData where
 readShape f = f . cdValShape
 writeShape f s = s { cdValShape = readShape f s }
 draw s = putStrLn $ mconcat
   [ "Drawing a Circle at:("
   , show (getX s) <> ","
   , show (getY s) <> "), radius "
   , show (cGetRadius s)
   ]

class Shape s => Circle s where
  cGetRadius :: s -> Int
  cGetRadius = readCircle cdValRadius

  cSetRadius :: Int -> s -> s
  cSetRadius i = writeCircle $ \s -> s { cdValRadius = i }

  readCircle  :: (CircleData -> t)          -> s -> t
  writeCircle :: (CircleData -> CircleData) -> s -> s

instance Circle CircleData where
  readCircle  = id
  writeCircle = id

main = do
  -- Such simple tagging was not possible with the tail-polymorphic encodings.
  let scribble =
        [ HideShape $ rectangle 10 20 5 6
        , HideShape $ circle 15 25 8
        ]
  forM_ scribble $ \x -> do
    draw x
    draw $ rMoveTo 100 100 x
