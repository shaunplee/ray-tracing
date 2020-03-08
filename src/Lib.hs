{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Data.Word (Word8)

type RGB = Vec3 Word8

type XYZ = Vec3 Double

data Vec3 a = Vec3
  { vecX :: a
  , vecY :: a
  , vecZ :: a
  }

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Show a => Show (Vec3 a) where
  show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z

instance (Floating a, Num a) => Num (Vec3 a) where
  (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (*) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  --this definition of abs is clearly wrong, as it should be a scalar
  --not a vector
  abs (Vec3 x y z) = Vec3 (sqrt (x * x + y * y + z * z)) 0 0
  signum v@(Vec3 x y z) =
    let (Vec3 m _ _) = abs v
     in Vec3 (x / m) (y / m) (z / m)
  fromInteger x = Vec3 (fromInteger x) 0 0

vecDiv :: Fractional a => Vec3 a -> Vec3 a -> Vec3 a
vecDiv (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

length :: (Floating a, Num a) => Vec3 a -> a
length v = let (Vec3 l _ _) = abs v in l

squaredLength :: (Num a) => Vec3 a -> a
squaredLength (Vec3 x y z) = x * x + y * y + z * z

makeUnitVector :: (Floating a) => Vec3 a -> Vec3 a
makeUnitVector = signum

scale :: (Floating a) => a -> Vec3 a -> Vec3 a
scale k (Vec3 x y z) = Vec3 (k * x) (k * y) (k * z)

divide :: (Floating a) => Vec3 a -> a -> Vec3 a
divide (Vec3 x y z) k = Vec3 (x / k) (y / k) (z / k)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)


r :: RGB -> Word8
r (Vec3 x _ _) = x

g :: RGB -> Word8
g (Vec3 _ y _) = y

b :: RGB -> Word8
b (Vec3 _ _ z) = z

scaleColor :: Double -> Word8
scaleColor x = floor (255.99 * x)

scaleColors :: Vec3 Double -> RGB
scaleColors = fmap scaleColor

printRow :: [RGB] -> IO ()
printRow row = putStrLn $ showRow row

showRow :: [RGB] -> String
showRow row = unwords $ fmap show row


data Ray a = Ray
  { origin    :: Vec3 a
  , direction :: Vec3 a
  } deriving Show

pointAtParameter :: Floating a => Ray a -> a -> Vec3 a
pointAtParameter r t = origin r + scale t (direction r)

hitSphere :: (Floating a, Num a, Ord a) => Vec3 a -> a -> Ray a -> Bool
hitSphere center radius ray =
  let oc = origin ray - center
      a = dot (direction ray) (direction ray)
      b = 2.0 * dot oc (direction ray)
      c = dot oc oc - (radius * radius)
      discriminant = b * b - 4 * a * c
  in discriminant > 0

color :: (Fractional a, Floating a) => Ray a -> Vec3 a
color r =
  let unitDirection = makeUnitVector (direction r)
      t = 0.5 * (vecY unitDirection + 1.0)
   in scale (1.0 - t) (Vec3 1.0 1.0 1.0) + scale t (Vec3 0.5 0.7 1.0)

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  let nx = 200
  let ny = 100
  putStrLn "P3"
  putStrLn $ show nx ++ " " ++ show ny
  putStrLn "255"
  let lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)
  let horizontal = Vec3 4.0 0.0 0.0
  let vertical = Vec3 0.0 2.0 0.0
  let o = Vec3 0.0 0.0 0.0
  let pp = pixelPositions nx ny
  let renderPos :: (Int, Int) -> Vec3 Double
      renderPos (x, y) =
        let u = fromIntegral x / fromIntegral nx
            v = fromIntegral y / fromIntegral ny
            r = Ray o (lowerLeftCorner + scale u horizontal + scale v vertical)
         in color r
  let vals = map (map (scaleColors . renderPos)) pp
  mapM_ printRow vals
