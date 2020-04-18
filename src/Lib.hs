{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (foldM)
import           Control.Monad.Reader
import           Control.Monad.ST              (ST (..), runST)
import           Control.Monad.Trans           (lift)
import           Control.Monad.Trans.State     (StateT (..), evalStateT, get,
                                                put, runStateT)
import           Data.Foldable                 (foldl')
import           Data.List                     (intercalate)
import           Data.Maybe                    (catMaybes)
import           Data.STRef
import           Data.Word                     (Word8)
import           System.IO                     (hPutStr, stderr)
import           System.Random.Mersenne.Pure64

-- Number of samples to use when anti-aliasing
ns :: Int
ns = 100

-- maximum number of reflections
maxDepth = 50

-- X and Y dimensions of output image
imageWidth :: Int
imageWidth = 600
imageHeight :: Int
imageHeight = 400

infinity :: Double
infinity = read "Infinity" :: Double

type World = [Shape]

-- Use the ST monad to thread the random number generator
-- Use the ReaderT monad to track global state
type RandomState s = StateT (STRef s PureMT) (ST s)

type RayTracingM s = ReaderT World (RandomState s)

-- Final representation of a color of a pixel before output
type RGB = Vec3 Word8

-- General 3-dimensional Doubles--could be color or vector or position
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
  show (Vec3 (x, y, z)) = unwords [show x, show y, show z]

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

clamp :: Double -> Double -> Double -> Double
clamp x min max =
  if | x < min -> min
     | x > max -> max
     | otherwise -> x

scaleColor :: Double -> Word8
scaleColor x = floor $ 256 * clamp (sqrt x) 0.0 0.999

scaleColors :: Vec3 Double -> RGB
scaleColors = fmap scaleColor

printRow :: (Int, [RGB]) -> IO ()
printRow (i, row) = do
  hPutStr stderr ("\rRendering row " ++ show i ++ " of " ++ show imageHeight)
  putStrLn $ showRow row

showRow :: [RGB] -> String
showRow row = unwords $ fmap show row


data Ray = Ray
  { origin    :: XYZ
  , direction :: XYZ
  } deriving Show

at :: Ray -> Double -> XYZ
at (Ray or dr) t = or + scale t dr

data Hit = Hit
  { hit_t         :: Double
  , hit_p         :: XYZ
  , hit_normal    :: XYZ -- vector normal to the surface of the object
                         -- at the point of the hit
  , hit_frontFace :: Bool -- did the ray hit the outer face of the
                          -- object?
  , hit_material  :: Material
  }

-- class Hittable b where
--   hit :: b -> Ray -> Double -> Double -> Maybe Hit

data Material
  = Lambertian Attenuation
  | Metal Attenuation Fuzz
  | Dielectric RefractiveIdx

newtype Attenuation = Attenuation (Vec3 Double)

newtype Fuzz = Fuzz Double

newtype RefractiveIdx = RefractiveIdx Double

data Shape = Sphere
  { sphere_center   :: Vec3 Double
  , sphere_radius   :: Double
  , sphere_material :: Material
  }

scatter :: Material -> Ray -> Hit -> RandomState s (Maybe (Ray, Attenuation))
scatter (Lambertian att) rin (Hit _ hp hn _ _) = do
  rUnit <- randomUnitVectorM
  let scatterDirection = hn + rUnit
  let scattered = Ray hp scatterDirection
  return $ Just (scattered, att)
scatter (Metal att (Fuzz fuzz)) rin (Hit _ hp hn _ _) = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector (direction rin)) hn
  let scattered = Ray hp (reflected + scale fuzz rUnit)
  return $ if dot (direction scattered) hn > 0.0
           then Just (scattered, att)
           else Nothing
scatter (Dielectric (RefractiveIdx ref_idx)) rin (Hit _ hp hn hff _) = do
  let attenuation = Vec3 (1.0, 1.0, 1.0)
  let etaiOverEtat =
        if hff
          then 1.0 / ref_idx
          else ref_idx
  let unitDirection = makeUnitVector (direction rin)
  let cosTheta = min (dot (-unitDirection) hn) 1.0
  let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
  rd <- randomDoubleM
  return $
    if (etaiOverEtat * sinTheta > 1.0) || rd < schlick cosTheta etaiOverEtat
      then let reflected = reflect unitDirection hn
            in Just (Ray hp reflected, Attenuation attenuation)
      else let refracted =
                 refract unitDirection hn etaiOverEtat
            in Just (Ray hp refracted, Attenuation attenuation)

reflect :: XYZ -> XYZ -> XYZ
reflect v n = v - scale (2.0 * dot v n) n

refract :: XYZ -> XYZ -> Double -> XYZ
refract v n etaiOverEtat =
  let uv = makeUnitVector v
      cosTheta = dot (-uv) n
      rOutParallel = scale etaiOverEtat (uv + scale cosTheta n)
      rOutPerp = scale (-sqrt (1.0 - squaredLength rOutParallel)) n
   in rOutParallel + rOutPerp

-- Christopher Schlick approximation for reflectivity of glass based on angle
schlick :: Double -> Double -> Double
schlick cos ref_idx =
  let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
      r1 = r0 * r0
   in r1 + (1.0 - r1) * (1 - cos) ** 5

hit :: Shape -> Ray -> Double -> Double -> Maybe Hit
hit sphere@(Sphere sc sr _) r@(Ray or dr) t_min t_max =
  let oc = or - sc
      a = dot dr dr
      b = seq oc (dot oc dr)
      c = seq sr (dot oc oc - (sr * sr))
      discriminant = b * b - a * c
   in if discriminant > 0
        then let sd = sqrt discriminant
                 temp1 = ((-b) - sd) / a
                 temp2 = ((-b) + sd) / a
              in if | t_min < temp1 && temp1 < t_max ->
                      Just $ recHit temp1 r sphere
                    | t_min < temp2 && temp2 < t_max ->
                      Just $ recHit temp2 r sphere
                    | otherwise -> Nothing
        else Nothing

recHit :: Double -> Ray -> Shape -> Hit
recHit temp r@(Ray or dr) sphere@(Sphere sc sr sm) =
  let p = r `at` temp
      outwardNormal = divide (p - sc) sr
      frontFace = dot dr outwardNormal < 0.0
      n =
        if frontFace
          then outwardNormal
          else -outwardNormal
   in Hit temp p n frontFace sm

hitList :: [Shape] -> Ray -> Double -> Double -> Maybe Hit
hitList htbls r t_min t_max =
  foldl'
    (\mh htbl ->
       case mh of
         Nothing -> hit htbl r t_min t_max
         Just (Hit ht _ _ _ _)  -> case hit htbl r t_min ht of
           Nothing -> mh
           h1      -> h1)
    Nothing
    htbls

hitSphere :: XYZ -> Double -> Ray -> Double
hitSphere center radius ray@(Ray or dr) =
  let oc = or - center
      a = dot dr dr
      b = 2.0 * dot oc dr
      c = dot oc oc - (radius * radius)
      discriminant = b * b - 4 * a * c
  in if discriminant < 0.0
     -- no hits
     then (-1.0)
     -- there's a hit
     else ((-b) - sqrt discriminant) / (2.0 * a)

randomDoubleM :: RandomState s Double
randomDoubleM = do
  gRef <- get
  g1 <- lift $ readSTRef gRef
  let (x, g2) = randomDouble g1
  lift $ writeSTRef gRef g2
  return x

randomDoubleRM :: Double -> Double -> RandomState s Double
randomDoubleRM min max = do
  rd <- randomDoubleM
  return $ min + (max - min) * rd

randomVec3DoubleM :: RandomState s (Vec3 Double)
randomVec3DoubleM = do
  gRef <- get
  gen <- lift $ readSTRef gRef
  let (x, g1) = randomDouble gen
  let (y, g2) = randomDouble g1
  let (z, g3) = randomDouble g2
  lift $ writeSTRef gRef g3
  return $ Vec3 (x, y, z)

randomVec3DoubleRM :: Double -> Double -> RandomState s (Vec3 Double)
randomVec3DoubleRM min max = do
  x <- randomDoubleRM min max
  y <- randomDoubleRM min max
  z <- randomDoubleRM min max
  return $ Vec3 (x, y, z)

randomInUnitSphereM :: RandomState s (Vec3 Double)
randomInUnitSphereM = do
  gRef <- get
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitSphere gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitSphere :: PureMT -> (Vec3 Double, PureMT)
randomInUnitSphere gen =
  let (x, g1) = randomDouble gen
      (y, g2) = randomDouble g1
      (z, g3) = randomDouble g2
      p = scale 2.0 (Vec3 (x, y, z)) - Vec3 (1.0, 1.0, 1.0)
   in if squaredLength p < 1.0
        then (p, g3)
        else randomInUnitSphere g3

randomInUnitDiskM :: RandomState s (Vec3 Double)
randomInUnitDiskM = do
  gRef <- get
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitDisk gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitDisk :: PureMT -> (Vec3 Double, PureMT)
randomInUnitDisk gen =
  let (x, g1) = randomDouble gen
      (y, g2) = randomDouble g1
      p = scale 2.0 (Vec3 (x, y, 0)) - Vec3 (1.0, 1.0, 0)
   in if squaredLength p < 1.0
        then (p, g2)
        else randomInUnitDisk g2

randomUnitVectorM :: RandomState s (Vec3 Double)
randomUnitVectorM = do
  gRef <- get
  gen <- lift $ readSTRef gRef
  let (aa, g1) = randomDouble gen
  let a = aa * 2 * pi
  let (zz, g2) = randomDouble g1
  let z = (zz * 2) - 1
  let r = sqrt (1 - z * z)
  lift $ writeSTRef gRef g2
  return $ Vec3 (r * cos a, r * sin a, z)

randomInHemisphereM :: XYZ -> RandomState s (Vec3 Double)
randomInHemisphereM n = do
  inUnitSphere <- randomInUnitSphereM
  if (inUnitSphere `dot` n) > 0.0
    then return inUnitSphere
    else return (-inUnitSphere)

data Camera = Camera
  { camera_origin     :: XYZ
  , camera_llc        :: XYZ
  , camera_horiz      :: XYZ
  , camera_vert       :: XYZ
  , camera_u          :: XYZ
  , camera_v          :: XYZ
  , camera_w          :: XYZ
  , camera_lensRadius :: Double
  }

getRay :: Camera -> Double -> Double -> RandomState s Ray
getRay c s t = do
  rd <- fmap (scale $ camera_lensRadius c) randomInUnitDiskM
  let offset = scale (vecX rd) (camera_u c) + scale (vecY rd) (camera_v c)
  return $
    Ray
      (camera_origin c + offset)
      (camera_llc c + scale s (camera_horiz c) + scale t (camera_vert c) -
       camera_origin c -
       offset)

defaultCamera :: Camera
defaultCamera =
  newCamera
    (Vec3 (13.0, 2.0, 3.0))
    (Vec3 (0.0, 0.0, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1

newCamera :: XYZ -> XYZ -> XYZ -> Double -> Double -> Double ->  Camera
newCamera lookfrom lookat vup vfov aspect aperture =
  let lensRadius = aperture / 2.0
      theta = vfov * pi / 180.0
      halfHeight = tan (theta / 2.0)
      halfWidth = aspect * halfHeight
      origin = lookfrom
      focusDist = Lib.length (lookfrom - lookat)
      w = makeUnitVector (lookfrom - lookat)
      u = makeUnitVector (cross vup w)
      v = cross w u
      lowerLeftCorner =
        origin - scale (halfWidth * focusDist) u -
        scale (halfHeight * focusDist) v -
        scale focusDist w
      horizontal = scale (2 * halfWidth * focusDist) u
      vertical = scale (2 * halfHeight * focusDist) v
   in Camera origin lowerLeftCorner horizontal vertical u v w lensRadius

rayColor :: Ray -> Int -> RayTracingM s (Vec3 Double)
rayColor r depth = rayColorHelp r depth (Attenuation $ Vec3 (1.0, 1.0, 1.0))
  where
    rayColorHelp :: Ray -> Int -> Attenuation -> RayTracingM s (Vec3 Double)
    rayColorHelp r depth (Attenuation att_acc) = do
      htbls <- ask
      if depth <= 0
        then return (Vec3 (0.0, 0.0, 0.0))
        else case hitList htbls r 0.001 infinity of
               Just h -> do
                 mscatter <- lift $ scatter (hit_material h) r h
                 case mscatter of
                   Just (sray, Attenuation att) ->
                     rayColorHelp sray (depth - 1) (Attenuation (att_acc * att))
                   Nothing -> return $ Vec3 (0.0, 0.0, 0.0)
               Nothing ->
                 let unitDirection = makeUnitVector (direction r)
                     t = 0.5 * (vecY unitDirection + 1.0)
                  in return $
                     att_acc *
                     (scale (1.0 - t) (Vec3 (1.0, 1.0, 1.0)) +
                      scale t (Vec3 (0.5, 0.7, 1.0)))

sampleColor :: (Int, Int) -> Vec3 Double -> Int -> RayTracingM s (Vec3 Double)
sampleColor (x, y) accCol _ = do
  gRef <- lift $ get
  gen <- lift $ lift $ readSTRef gRef
  let (ru, g1) = randomDouble gen
  let (rv, g2) = randomDouble g1
  let u = (fromIntegral x + ru) / fromIntegral imageWidth
  let v = (fromIntegral y + rv) / fromIntegral imageHeight
  r <- lift $ getRay defaultCamera u v
  lift $ lift $ writeSTRef gRef g2
  c1 <- rayColor r maxDepth
  return $ accCol + c1

renderPos :: (Int, Int) -> RayTracingM s RGB
renderPos (x, y) = do
  summedColor <- foldM (sampleColor (x, y)) (Vec3 (0.0, 0.0, 0.0)) [0 .. ns - 1]
  return $ scaleColors (divide summedColor (fromIntegral ns))

renderRow :: [(Int, Int)] -> RayTracingM s [RGB]
renderRow ps = mapM renderPos ps

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  let pp = pixelPositions imageWidth imageHeight
  gen <- newPureMT
  let (world, g1) = makeWorld gen
  let vals = zip [1 .. imageHeight] pp
  mapM_
    (printRow .
     (\(i, rm) ->
        let r :: [RGB]
            r =
              runST $ do
                gRef <- newSTRef g1
                evalStateT (runReaderT (renderRow rm) world) gRef
         in (i, r)))
    vals
  hPutStr stderr "\nDone.\n"

-- |Generate the image from the cover of the book with lots of spheres
makeWorld :: PureMT -> (World, PureMT)
makeWorld gen =
  runST $ do
    gRef <- newSTRef gen
    world <- evalStateT makeWorldM gRef
    g1 <- readSTRef gRef
    return (world, g1)
  where
    makeWorldM :: RandomState s World
    makeWorldM = do
      let ns = [(x, y) | x <- [-11 .. 10], y <- [-11 .. 10]]
      let ground =
            Sphere
              (Vec3 (0.0, -1000.0, 0.0))
              1000
              (Lambertian (Attenuation $ Vec3 (0.5, 0.5, 0.5)))
      let s1 =
            Sphere (Vec3 (0.0, 1.0, 0.0)) 1.0 (Dielectric (RefractiveIdx 1.5))
      let s2 =
            Sphere
              (Vec3 (-4.0, 1.0, 0.0))
              1.0
              (Lambertian (Attenuation $ Vec3 (0.4, 0.2, 0.1)))
      let s3 =
            Sphere
              (Vec3 (4.0, 1.0, 0.0))
              1.0
              (Metal (Attenuation $ Vec3 (0.7, 0.6, 0.5)) (Fuzz 0.0))
      nps <- catMaybes <$> mapM makeRandomSphereM ns
      return $ ground : s1 : s2 : s3 : nps
    makeRandomSphereM :: (Int, Int) -> RandomState s (Maybe Shape)
    makeRandomSphereM (a, b) = do
      mat <- randomDoubleM
      px <- randomDoubleM
      py <- randomDoubleM
      let center =
            Vec3 (fromIntegral a + 0.9 * px, 0.2, fromIntegral b + 0.9 * py)
      if Lib.length (center - Vec3 (4.0, 0.2, 0)) <= 0.9
        then return Nothing
        else if | mat < 0.8 -- Diffuse
                 ->
                  do a1 <- randomVec3DoubleM
                     a2 <- randomVec3DoubleM
                     let albedo = a1 * a2
                     return $
                       Just $
                       Sphere center 0.2 (Lambertian (Attenuation albedo))
                | mat < 0.95 -- Metal
                 ->
                  do albedo <- randomVec3DoubleRM 0.5 1.0
                     fuzz <- randomDoubleRM 0.0 0.5
                     return $
                       Just $
                       Sphere
                         center
                         0.2
                         (Metal (Attenuation albedo) (Fuzz fuzz))
                | otherwise --Glass
                 ->
                  return $
                  Just $ Sphere center 0.2 (Dielectric (RefractiveIdx 1.5))
