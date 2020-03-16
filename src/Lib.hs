{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Data.Foldable (foldl')
import           Data.Word     (Word8)

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


data Ray = Ray
  { origin    :: XYZ
  , direction :: XYZ
  } deriving Show

data Hit = Hit
  { hit_t      :: Double
  , hit_p      :: XYZ
  , hit_normal :: XYZ
  }

class Hittable b where
  hit :: b -> Ray -> Double -> Double -> Maybe Hit

data Sphere = Sphere
  { sphere_center :: Vec3 Double
  , sphere_radius :: Double
  }

instance Hittable Sphere where
  hit sphere r t_min t_max =
    let oc = origin r - sphere_center sphere
        a = dot (direction r) (direction r)
        b = dot oc (direction r)
        c = dot oc oc - (sphere_radius sphere * sphere_radius sphere)
        discriminant = b * b - a * c
     in if discriminant > 0
          then let temp1 = ((-b) - sqrt discriminant) / a
                   temp2 = ((-b) + sqrt discriminant) / a
                in if | temp1 < t_max && temp1 > t_min -> Just $ recHit temp1
                      | temp2 < t_max && temp2 > t_min -> Just $ recHit temp2
                      | otherwise -> Nothing
          else Nothing
    where
      recHit temp =
        let p = pointAtParameter r temp
            n = divide (p - sphere_center sphere) (sphere_radius sphere)
         in Hit temp p n

hitList :: Hittable a => [a] -> Ray -> Double -> Double -> Maybe Hit
hitList htbls r t_min t_max =
  foldl'
    (\mh htbl ->
       case mh of
         Nothing -> hit htbl r t_min t_max
         Just h  -> case hit htbl r t_min (hit_t h) of
           Nothing -> mh
           h1      -> h1)
    Nothing
    htbls

pointAtParameter :: Ray -> Double -> XYZ
pointAtParameter r t = origin r + scale t (direction r)

hitSphere :: XYZ -> Double -> Ray -> Double
hitSphere center radius ray =
  let oc = origin ray - center
      a = dot (direction ray) (direction ray)
      b = 2.0 * dot oc (direction ray)
      c = dot oc oc - (radius * radius)
      discriminant = b * b - 4 * a * c
  in if discriminant < 0.0
     then (-1.0)
     else ((-b) - sqrt discriminant) / (2.0 * a)

color :: Hittable a => Ray -> [a] -> Vec3 Double
color r htbls =
  case hitList htbls r 0.0 (read "Infinity" :: Double) of
    Just h -> scale 0.5 (hit_normal h + Vec3 1.0 1.0 1.0)
    Nothing ->
      let unitDirection = makeUnitVector (direction r)
          t = 0.5 * (vecY unitDirection + 1.0)
       in scale (1.0 - t) (Vec3 1.0 1.0 1.0) + scale t (Vec3 0.5 0.7 1.0)

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  let nx = 400
  let ny = 200
  putStrLn "P3"
  putStrLn $ show nx ++ " " ++ show ny
  putStrLn "255"
  let lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)
  let horizontal = Vec3 4.0 0.0 0.0
  let vertical = Vec3 0.0 2.0 0.0
  let o = Vec3 0.0 0.0 0.0
  let pp = pixelPositions nx ny
  let world =
        [ Sphere (Vec3 0.0 0.0 (-1.0)) 0.5
        , Sphere (Vec3 0.0 (-100.5) (-1.0)) 100]
  let renderPos :: (Int, Int) -> Vec3 Double
      renderPos (x, y) =
        let u = fromIntegral x / fromIntegral nx
            v = fromIntegral y / fromIntegral ny
            r = Ray o (lowerLeftCorner + scale u horizontal + scale v vertical)
         in color r world
  let vals = map (map (scaleColors . renderPos)) pp
  mapM_ printRow vals
