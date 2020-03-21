{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad.State.Strict (State (..), evalState, get, put,
                                             runState)
import           Data.Foldable              (foldl')
import           Data.Word                  (Word8)
import           System.Random

-- Number of samples to use when anti-aliasing
ns :: Int
ns = 100

-- X and Y dimensions of output image
nx :: Int
nx = 200
ny :: Int
ny = 100

world :: [Shape]
world =
  [Sphere (Vec3 (0.0, 0.0, -1.0)) 0.5, Sphere (Vec3 (0.0, -100.5, -1.0)) 100]

type RandomState = State StdGen

type RGB = Vec3 Word8

type XYZ = Vec3 Double

newtype Vec3 a = Vec3 (a, a, a)

vecX :: Vec3 a -> a
vecX (Vec3 (x, _ , _)) = x

vecY :: Vec3 a -> a
vecY (Vec3 (_, y, _)) = y

vecZ :: Vec3 a -> a
vecZ (Vec3 (_, _, z)) = z

instance Functor Vec3 where
  fmap f (Vec3 (x, y, z)) = Vec3 (f x, f y, f z)

instance Show a => Show (Vec3 a) where
  show (Vec3 (x, y, z)) = show x ++ " " ++ show y ++ " " ++ show z

instance (Floating a, Num a) => Num (Vec3 a) where
  (+) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
    Vec3 (x1 + x2, y1 + y2, z1 + z2)
  (-) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
    Vec3 (x1 - x2, y1 - y2, z1 - z2)
  (*) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
    Vec3 (x1 * x2, y1 * y2, z1 * z2)
  negate (Vec3 (x, y, z)) = Vec3 (-x, -y, -z)
  --this definition of abs is clearly wrong, as it should be a scalar
  --not a vector
  abs (Vec3 (x, y, z)) = Vec3 (sqrt (x * x + y * y + z * z), 0, 0)
  signum v@(Vec3 (x, y, z)) =
    let (Vec3 (m, _, _)) = abs v
     in Vec3 (x / m, y / m, z / m)
  fromInteger x = Vec3 (fromInteger x, 0, 0)

vecDiv :: Fractional a => Vec3 a -> Vec3 a -> Vec3 a
vecDiv (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (x1 / x2, y1 / y2, z1 / z2)

length :: (Floating a, Num a) => Vec3 a -> a
length v = let (Vec3 (l, _, _)) = abs v in l

squaredLength :: (Num a) => Vec3 a -> a
squaredLength (Vec3 (x, y, z)) = x * x + y * y + z * z

makeUnitVector :: (Floating a) => Vec3 a -> Vec3 a
makeUnitVector = signum

scale :: (Floating a) => a -> Vec3 a -> Vec3 a
scale k (Vec3 (x, y, z)) = Vec3 (k * x, k * y, k * z)

divide :: (Floating a) => Vec3 a -> a -> Vec3 a
divide (Vec3 (x, y, z)) k = Vec3 (x / k, y / k, z / k)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)


rgbr :: RGB -> Word8
rgbr (Vec3 (x, _, _)) = x

rgbg :: RGB -> Word8
rgbg (Vec3 (_, y, _)) = y

rgbb :: RGB -> Word8
rgbb (Vec3 (_, _, z)) = z

scaleColor :: Double -> Word8
scaleColor x = floor (255.99 * sqrt x)

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

data Shape = Sphere
  { sphere_center :: Vec3 Double
  , sphere_radius :: Double
  }

instance Hittable Shape where
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

randomInUnitSphere :: RandomGen g => g -> (Vec3 Double, g)
randomInUnitSphere gen =
  let (x, g1) = random gen
      (y, g2) = random g1
      (z, g3) = random g2
      p = scale 2.0 (Vec3 (x, y, z)) - Vec3 (1.0, 1.0, 1.0)
   in if squaredLength p < 1.0
        then (p, g3)
        else randomInUnitSphere g3

data Camera = Camera
  { camera_origin :: XYZ
  , camera_llc    :: XYZ
  , camera_horiz  :: XYZ
  , camera_vert   :: XYZ
  }

getRay :: Camera -> Double -> Double -> Ray
getRay c u v =
  Ray
    (camera_origin c)
    (camera_llc c + scale u (camera_horiz c) + scale v (camera_vert c) -
     camera_origin c)

defaultCamera :: Camera
defaultCamera = let lowerLeftCorner = Vec3 (-2.0, -1.0, -1.0)
                    horizontal      = Vec3 (4.0, 0.0, 0.0)
                    vertical        = Vec3 (0.0, 2.0, 0.0)
                    origin          = Vec3 (0.0, 0.0, 0.0)
                 in Camera origin lowerLeftCorner horizontal vertical

color :: (RandomGen g, Hittable a) => g -> Ray -> [a] -> Vec3 Double
color gen r htbls =
  case hitList htbls r 0.001 (read "Infinity" :: Double) of
    Just h ->
      let (rv, g1) = randomInUnitSphere gen
          target = hit_p h + hit_normal h + rv
          c2 = color g1 (Ray (hit_p h) (target - hit_p h)) htbls
       in scale 0.5 c2
    Nothing ->
      let unitDirection = makeUnitVector (direction r)
          t = 0.5 * (vecY unitDirection + 1.0)
       in scale (1.0 - t) (Vec3 (1.0, 1.0, 1.0)) +
          scale t (Vec3 (0.5, 0.7, 1.0))

sampleColor ::
     RandomGen rg => (Int, Int) -> (Vec3 Double, rg) -> Int -> (Vec3 Double, rg)
sampleColor (x, y) (accCol, gen) _ =
  let (ru, g1) = random gen
      (rv, g2) = random g1
      u = (fromIntegral x + ru) / fromIntegral nx
      v = (fromIntegral y + rv) / fromIntegral ny
      r = getRay defaultCamera u v
      (g3, g4) = split g2
      c1 = color g3 r world
   in (accCol + c1, g4)

renderPos :: (Int, Int) -> RandomState RGB
renderPos (x, y) = do
  gen <- get
  let (summedColor, g1) =
        foldr
          (flip $ sampleColor (x, y))
          (Vec3 (0.0, 0.0, 0.0), gen)
          [0 .. ns - 1]
  put g1
  return $ scaleColors (divide summedColor (fromIntegral ns))

renderRow :: [(Int, Int)] -> RandomState [RGB]
renderRow ps = do
  gen <- get
  let (r, g1) = runState (mapM renderPos ps) gen
  put g1
  return r

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  putStrLn "P3"
  putStrLn $ show nx ++ " " ++ show ny
  putStrLn "255"
  let cam = defaultCamera
  let pp = pixelPositions nx ny
  gen <- getStdGen
  let vals = evalState (mapM renderRow pp) gen
  mapM_ printRow vals
