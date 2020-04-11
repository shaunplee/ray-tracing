{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad                 (foldM)
import           Control.Monad.State.Strict    (State (..), evalState, get, put,
                                                runState)
import           Data.Foldable                 (foldl')
import           Data.List                     (intercalate)
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
imageWidth = 400
imageHeight :: Int
imageHeight = 200

infinity :: Double
infinity = read "Infinity" :: Double

rWorld :: Double
rWorld = cos (pi / 4.0)

-- The shapes in the world to be rendered
world :: [Shape]
world =
  [ Sphere
      (Vec3 (0.0, 0.0, -1.0))
      0.5
      (Lambertian (Attenuation $ Vec3 (0.1, 0.2, 0.5)))
  , Sphere
      (Vec3 (0.0, -100.5, -1.0))
      100
      (Lambertian (Attenuation $ Vec3 (0.8, 0.8, 0.0)))
  , Sphere
      (Vec3 (1.0, 0.0, -1.0))
      0.5
      (Metal (Attenuation $ Vec3 (0.8, 0.6, 0.2)) (Fuzz 0.3))
  , Sphere (Vec3 (-1.0, 0.0, -1.0)) 0.5 (Dielectric (RefractiveIdx 1.5))
  , Sphere (Vec3 (-1.0, 0.0, -1.0)) (-0.45) (Dielectric (RefractiveIdx 1.5))
  ]

-- Use the State monad to thread the random number generator
type RandomState = State PureMT

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
  show (Vec3 (x, y, z)) = intercalate " " [show x, show y, show z]

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
at r t = origin r + scale t (direction r)

data Hit = Hit
  { hit_t         :: Double
  , hit_p         :: XYZ
  , hit_normal    :: XYZ -- vector normal to the surface of the object
                         -- at the point of the hit
  , hit_frontFace :: Bool -- did the ray hit the outer face of the
                          -- object?
  , hit_material  :: Material
  }

class Hittable b where
  hit :: b -> Ray -> Double -> Double -> Maybe Hit

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

scatter :: Material -> Ray -> Hit -> RandomState (Maybe (Ray, Attenuation))
scatter (Lambertian att) rin hit_rec = do
  rUnit <- randomUnitVectorM
  let target = hit_p hit_rec + hit_normal hit_rec + rUnit
  return $ Just (Ray (hit_p hit_rec) (target - hit_p hit_rec), att)
scatter (Metal att (Fuzz fuzz)) rin hit_rec = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector (direction rin)) (hit_normal hit_rec)
  let scattered = Ray (hit_p hit_rec) (reflected + scale fuzz rUnit)
  return $ if dot (direction scattered) (hit_normal hit_rec) > 0.0
           then Just (scattered, att)
           else Nothing
scatter (Dielectric (RefractiveIdx ref_idx)) rin hit_rec = do
  let attenuation = Vec3 (1.0, 1.0, 1.0)
  let etaiOverEtat =
        if hit_frontFace hit_rec
          then 1.0 / ref_idx
          else ref_idx
  let unitDirection = makeUnitVector (direction rin)
  let cosTheta = min (dot (-unitDirection) (hit_normal hit_rec)) 1.0
  let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
  rd <- randomDoubleM
  return $
    if etaiOverEtat * sinTheta > 1.0
      then let reflected = reflect unitDirection (hit_normal hit_rec)
            in Just (Ray (hit_p hit_rec) reflected, Attenuation attenuation)
      else if rd < schlick cosTheta etaiOverEtat
             then let reflected = reflect unitDirection (hit_normal hit_rec)
                   in Just
                        (Ray (hit_p hit_rec) reflected, Attenuation attenuation)
             else let refracted =
                        refract unitDirection (hit_normal hit_rec) etaiOverEtat
                   in Just
                        (Ray (hit_p hit_rec) refracted, Attenuation attenuation)

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

instance Hittable Shape where
  hit sphere r t_min t_max =
    let oc = origin r - sphere_center sphere
        a = dot (direction r) (direction r)
        b = dot oc (direction r)
        c = dot oc oc - (sphere_radius sphere * sphere_radius sphere)
        discriminant = b * b - a * c
     in if discriminant > 0
          then let sd = sqrt discriminant
                   temp1 = ((-b) - sd) / a
                   temp2 = ((-b) + sd) / a
                in if | temp1 < t_max && temp1 > t_min -> Just $ recHit temp1
                      | temp2 < t_max && temp2 > t_min -> Just $ recHit temp2
                      | otherwise -> Nothing
          else Nothing
    where
      recHit temp =
        let p = r `at` temp
            outwardNormal =
              divide (p - sphere_center sphere) (sphere_radius sphere)
            frontFace = dot (direction r) outwardNormal < 0.0
            n = if frontFace then outwardNormal else -outwardNormal
         in Hit temp p n frontFace (sphere_material sphere)

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

hitSphere :: XYZ -> Double -> Ray -> Double
hitSphere center radius ray =
  let oc = origin ray - center
      a = dot (direction ray) (direction ray)
      b = 2.0 * dot oc (direction ray)
      c = dot oc oc - (radius * radius)
      discriminant = b * b - 4 * a * c
  in if discriminant < 0.0
     -- no hits
     then (-1.0)
     -- there's a hit
     else ((-b) - sqrt discriminant) / (2.0 * a)

randomDoubleM :: RandomState Double
randomDoubleM = do
  g1 <- get
  let (x, g2) = randomDouble g1
  put g2
  return x

randomDoubleRM :: Double -> Double -> RandomState Double
randomDoubleRM min max = do
  rd <- randomDoubleM
  return $ min + (max - min) * rd

randomInUnitSphereM :: RandomState (Vec3 Double)
randomInUnitSphereM = do
  gen <- get
  let (rUnit, gen1) = randomInUnitSphere gen
  put gen1
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

randomUnitVectorM :: RandomState (Vec3 Double)
randomUnitVectorM = do
  gen <- get
  let (aa, g1) = randomDouble gen
  let a = aa * 2 * pi
  let (zz, g2) = randomDouble g1
  let z = (zz * 2) - 1
  let r = sqrt (1 - z * z)
  put g2
  return $ Vec3 (r * cos a, r * sin a, z)

randomInHemisphereM :: XYZ -> RandomState (Vec3 Double)
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

getRay :: Camera -> Double -> Double -> RandomState Ray
getRay c u v = do
  rd <- fmap (scale (camera_lensRadius c)) randomInUnitSphereM
  let offset = scale (vecX rd) (camera_u c) + scale (vecY rd) (camera_v c)
  return $
    Ray
      (offset + camera_origin c)
      (camera_llc c + scale u (camera_horiz c) + scale v (camera_vert c) -
       offset -
       camera_origin c)

defaultCamera :: Camera
defaultCamera =
  newCamera
    (Vec3 (3.0, 3.0, 2.0))
    (Vec3 (0.0, 0.0, -1.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    2.0

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

rayColor :: (Hittable a) => Ray -> [a] -> Int -> RandomState (Vec3 Double)
rayColor r htbls depth =
  rayColorHelp r htbls depth (Attenuation $ Vec3 (1.0, 1.0, 1.0))
  where
    rayColorHelp ::
         (Hittable a)
      => Ray
      -> [a]
      -> Int
      -> Attenuation
      -> RandomState (Vec3 Double)
    rayColorHelp r htbls depth (Attenuation att_acc) =
      case hitList htbls r 0.001 infinity of
        Just h -> do
          gen <- get
          if depth <= 0
            then return (Vec3 (0.0, 0.0, 0.0))
            else do
              mscatter <- scatter (hit_material h) r h
              case mscatter of
                Just (sray, Attenuation att) ->
                  rayColorHelp
                    sray
                    htbls
                    (depth - 1)
                    (Attenuation (att_acc * att))
                Nothing -> return $ Vec3 (0.0, 0.0, 0.0)
        Nothing ->
          let unitDirection = makeUnitVector (direction r)
              t = 0.5 * (vecY unitDirection + 1.0)
           in return $
              att_acc *
              (scale (1.0 - t) (Vec3 (1.0, 1.0, 1.0)) +
               scale t (Vec3 (0.5, 0.7, 1.0)))

sampleColor :: (Int, Int) -> Vec3 Double -> Int -> RandomState (Vec3 Double)
sampleColor (x, y) accCol _ = do
  gen <- get
  let (ru, g1) = randomDouble gen
  let (rv, g2) = randomDouble g1
  let u = (fromIntegral x + ru) / fromIntegral imageWidth
  let v = (fromIntegral y + rv) / fromIntegral imageHeight
  r <- getRay defaultCamera u v
  put g2
  c1 <- rayColor r world maxDepth
  return $ accCol + c1

renderPos :: (Int, Int) -> RandomState RGB
renderPos (x, y) = do
  gen <- get
  let (summedColor, g1) =
        runState
          (foldM (sampleColor (x, y)) (Vec3 (0.0, 0.0, 0.0)) [0 .. ns - 1])
          gen
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
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  let pp = pixelPositions imageWidth imageHeight
  gen <- newPureMT
  let vals = evalState (mapM renderRow pp) gen
  mapM_ printRow (zip [1 .. imageHeight] vals)
  hPutStr stderr "\nDone.\n"
