{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

type RGB = Vec3 Int

type XYZ = Vec3 Double

data Vec3 a = Vec3 a a a

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


r :: RGB -> Int
r (Vec3 x _ _) = x

g :: RGB -> Int
g (Vec3 _ y _) = y

b :: RGB -> Int
b (Vec3 _ _ z) = z

x :: XYZ -> Double
x (Vec3 x _ _) = x

y :: XYZ -> Double
y (Vec3 _ y _) = y

z :: XYZ -> Double
z (Vec3 _ _ z) = z

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0..nx-1]) [ny-1, ny-2..0]
--pixelPositions nx ny = map (\y -> map (, y) [0..nx-1]) [0..ny-1]

posToRGB :: Int -> Int -> (Int, Int) -> Vec3 Double
posToRGB nx ny (x, y) = let r = fromIntegral x / fromIntegral nx
                            g = fromIntegral y / fromIntegral ny
                            b = 0.2
                        in Vec3 r g b

scaleColor :: Double -> Int
scaleColor x = round (255.99 * x)

printRow :: [Vec3 Int] -> IO ()
printRow row = putStrLn $ showRow row

showRow :: [Vec3 Int] -> String
showRow row = unwords $ (fmap show) row


data Ray a = Ray
  { origin    :: Vec3 a
  , direction :: Vec3 a
  } deriving Show


pointAtParameter :: Floating a => Ray a -> a -> Vec3 a
pointAtParameter r t = origin r + scale t (direction r)

someFunc :: IO ()
someFunc = do
  let nx = 200
  let ny = 100
  let pp = pixelPositions nx ny
  let vals = map (map (fmap scaleColor . posToRGB nx ny)) pp
  putStrLn "P3"
  putStrLn $ show nx ++ " " ++ show ny
  putStrLn "255"
  mapM_ printRow vals
