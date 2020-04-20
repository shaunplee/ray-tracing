{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (foldM)
import           Control.Monad.Reader
import           Control.Monad.ST.Lazy         (ST (..), runST)
import           Control.Monad.Trans           (lift)
import           Data.Foldable                 (foldl')
import           Data.List                     (intercalate)
import           Data.Maybe                    (catMaybes)
import           Data.STRef.Lazy
import           Data.Word                     (Word8)
import           System.IO                     (hPutStr, stderr)
import           System.Random.Mersenne.Pure64

-- Number of threads to use when rendering
nThreads :: Int
nThreads = 2

-- Number of samples to use when anti-aliasing
ns :: Int
ns = 50

nsPerThread :: Int
nsPerThread = ns `div` nThreads

-- maximum number of reflections
maxDepth :: Int
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
type RandomState s = ReaderT (STRef s World, STRef s PureMT) (ST s)

getGenRef :: RandomState s (STRef s PureMT)
getGenRef = do (_, genRef) <- ask
               return genRef

getWorldRef :: RandomState s (STRef s World)
getWorldRef = do (worldRef, _) <- ask
                 return worldRef

type RayTracingM s = RandomState s

-- Final representation of a color of a pixel before output
newtype RGB = RGB (Word8, Word8, Word8)

instance Show RGB where
  show (RGB (r, g, b)) = unwords [show r, show g, show b]

-- General 3-dimensional Doubles--could be color or vector or position
type XYZ = Vec3

newtype Vec3 = Vec3 (Double, Double, Double)

vecX :: Vec3 -> Double
vecX (Vec3 (x, _ , _)) = x

vecY :: Vec3 -> Double
vecY (Vec3 (_, y, _)) = y

vecZ :: Vec3 -> Double
vecZ (Vec3 (_, _, z)) = z

instance Show Vec3 where
  show (Vec3 (x, y, z)) = unwords [show x, show y, show z]

vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (x1 * x2, y1 * y2, z1 * z2)

vecDiv :: Vec3 -> Vec3 -> Vec3
vecDiv (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (x1 / x2, y1 / y2, z1 / z2)

vecAdd :: Vec3 -> Vec3 -> Vec3
vecAdd (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (x1 + x2, y1 + y2, z1 + z2)

vecSub :: Vec3 -> Vec3 -> Vec3
vecSub (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (x1 - x2, y1 - y2, z1 - z2)

vecNegate :: Vec3 -> Vec3
vecNegate (Vec3 (x, y, z)) = Vec3 (-x, -y, -z)

length :: Vec3 -> Double
length (Vec3 (x, y, z)) = sqrt (x * x + y * y + z * z)

squaredLength :: Vec3 -> Double
squaredLength (Vec3 (x, y, z)) = x * x + y * y + z * z

makeUnitVector :: Vec3 -> Vec3
makeUnitVector v@(Vec3 (x, y, z)) =
  let m = Lib.length v
   in Vec3 (x / m, y / m, z / m)

scale :: Double -> Vec3 -> Vec3
scale k (Vec3 (x, y, z)) = Vec3 (k * x, k * y, k * z)

divide :: Vec3 -> Double -> Vec3
divide (Vec3 (x, y, z)) k = Vec3 (x / k, y / k, z / k)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  Vec3 (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

clamp :: Double -> Double -> Double -> Double
clamp x min max =
  if | x < min -> min
     | x > max -> max
     | otherwise -> x

scaleColor :: Double -> Word8
scaleColor x = floor $ 256 * clamp (sqrt x) 0.0 0.999

scaleColors :: Vec3 -> RGB
scaleColors (Vec3 (x, y, z)) = RGB (scaleColor x, scaleColor y, scaleColor z)

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
at (Ray or dr) t = or `vecAdd` scale t dr

data Hit = Hit
  { hit_t         :: Double
  , hit_p         :: XYZ
  , hit_normal    :: XYZ -- vector normal to the surface of the object
                         -- at the point of the hit
  , hit_frontFace :: Bool -- did the ray hit the outer face of the
                          -- object?
  , hit_material  :: Material
  }

data Material
  = Lambertian Attenuation
  | Metal Attenuation Fuzz
  | Dielectric RefractiveIdx

newtype Attenuation = Attenuation Vec3

newtype Fuzz = Fuzz Double

newtype RefractiveIdx = RefractiveIdx Double

data Shape = Sphere
  { sphere_center   :: Vec3
  , sphere_radius   :: Double
  , sphere_material :: Material
  }

scatter :: Material -> Ray -> Hit -> RandomState s (Maybe (Ray, Attenuation))
scatter (Lambertian att) rin (Hit _ hp hn _ _) = do
  rUnit <- randomUnitVectorM
  let scatterDirection = hn `vecAdd` rUnit
  let scattered = Ray hp scatterDirection
  return $ Just (scattered, att)
scatter (Metal att (Fuzz fuzz)) rin (Hit _ hp hn _ _) = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector (direction rin)) hn
  let scattered = Ray hp (reflected `vecAdd` scale fuzz rUnit)
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
  let cosTheta = min (dot (vecNegate unitDirection) hn) 1.0
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
reflect v n = v `vecSub` scale (2.0 * dot v n) n

refract :: XYZ -> XYZ -> Double -> XYZ
refract v n etaiOverEtat =
  let uv = makeUnitVector v
      cosTheta = dot (vecNegate uv) n
      rOutParallel = scale etaiOverEtat (uv `vecAdd` scale cosTheta n)
      rOutPerp = scale (-sqrt (1.0 - squaredLength rOutParallel)) n
   in rOutParallel `vecAdd` rOutPerp

-- Christopher Schlick approximation for reflectivity of glass based on angle
schlick :: Double -> Double -> Double
schlick cos ref_idx =
  let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
      r1 = r0 * r0
   in r1 + (1.0 - r1) * (1 - cos) ** 5

hit :: Shape -> Ray -> Double -> Double -> Maybe Hit
hit sphere@(Sphere sc sr _) r@(Ray or dr) t_min t_max =
  let oc = or `vecSub` sc
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
      outwardNormal = divide (p `vecSub` sc) sr
      frontFace = dot dr outwardNormal < 0.0
      n =
        if frontFace
          then outwardNormal
          else vecNegate outwardNormal
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
  let oc = or `vecSub` center
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
  gRef <- getGenRef
  g1 <- lift $ readSTRef gRef
  let (x, g2) = randomDouble g1
  lift $ writeSTRef gRef g2
  return x

randomDoubleRM :: Double -> Double -> RandomState s Double
randomDoubleRM min max = do
  rd <- randomDoubleM
  return $ min + (max - min) * rd

randomVec3DoubleM :: RandomState s Vec3
randomVec3DoubleM = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (x, g1) = randomDouble gen
  let (y, g2) = randomDouble g1
  let (z, g3) = randomDouble g2
  lift $ writeSTRef gRef g3
  return $ Vec3 (x, y, z)

randomVec3DoubleRM :: Double -> Double -> RandomState s Vec3
randomVec3DoubleRM min max = do
  x <- randomDoubleRM min max
  y <- randomDoubleRM min max
  z <- randomDoubleRM min max
  return $ Vec3 (x, y, z)

randomInUnitSphereM :: RandomState s Vec3
randomInUnitSphereM = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitSphere gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitSphere :: PureMT -> (Vec3, PureMT)
randomInUnitSphere gen =
  let (x, g1) = randomDouble gen
      (y, g2) = randomDouble g1
      (z, g3) = randomDouble g2
      p = scale 2.0 (Vec3 (x, y, z)) `vecSub` Vec3 (1.0, 1.0, 1.0)
   in if squaredLength p < 1.0
        then (p, g3)
        else randomInUnitSphere g3

randomInUnitDiskM :: RandomState s Vec3
randomInUnitDiskM = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitDisk gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitDisk :: PureMT -> (Vec3, PureMT)
randomInUnitDisk gen =
  let (x, g1) = randomDouble gen
      (y, g2) = randomDouble g1
      p = scale 2.0 (Vec3 (x, y, 0)) `vecSub` Vec3 (1.0, 1.0, 0)
   in if squaredLength p < 1.0
        then (p, g2)
        else randomInUnitDisk g2

randomUnitVectorM :: RandomState s Vec3
randomUnitVectorM = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (aa, g1) = randomDouble gen
  let a = aa * 2 * pi
  let (zz, g2) = randomDouble g1
  let z = (zz * 2) - 1
  let r = sqrt (1 - z * z)
  lift $ writeSTRef gRef g2
  return $ Vec3 (r * cos a, r * sin a, z)

randomInHemisphereM :: XYZ -> RandomState s Vec3
randomInHemisphereM n = do
  inUnitSphere <- randomInUnitSphereM
  if (inUnitSphere `dot` n) > 0.0
    then return inUnitSphere
    else return (vecNegate inUnitSphere)

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
  let offset =
        scale (vecX rd) (camera_u c) `vecAdd` scale (vecY rd) (camera_v c)
  return $
    Ray
      (camera_origin c `vecAdd` offset)
      (camera_llc c `vecAdd` scale s (camera_horiz c) `vecAdd`
       scale t (camera_vert c) `vecSub`
       camera_origin c `vecSub`
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
    10.0

newCamera :: XYZ -> XYZ -> XYZ -> Double -> Double -> Double -> Double -> Camera
newCamera lookfrom lookat vup vfov aspect aperture focusDist =
  let lensRadius = aperture / 2.0
      theta = vfov * pi / 180.0
      halfHeight = tan (theta / 2.0)
      halfWidth = aspect * halfHeight
      origin = lookfrom
      w = makeUnitVector (lookfrom `vecSub` lookat)
      u = makeUnitVector (cross vup w)
      v = cross w u
      lowerLeftCorner =
        origin `vecSub` scale (halfWidth * focusDist) u `vecSub`
        scale (halfHeight * focusDist) v `vecSub`
        scale focusDist w
      horizontal = scale (2 * halfWidth * focusDist) u
      vertical = scale (2 * halfHeight * focusDist) v
   in Camera origin lowerLeftCorner horizontal vertical u v w lensRadius

rayColor :: Ray -> Int -> RayTracingM s Vec3
rayColor r depth = rayColorHelp r depth (Attenuation $ Vec3 (1.0, 1.0, 1.0))
  where
    rayColorHelp :: Ray -> Int -> Attenuation -> RayTracingM s Vec3
    rayColorHelp r depth (Attenuation att_acc) = do
      worldRef <- getWorldRef
      htbls <- lift $ readSTRef worldRef
      if depth <= 0
        then return (Vec3 (0.0, 0.0, 0.0))
        else case hitList htbls r 0.001 infinity of
               Just h -> do
                 mscatter <- scatter (hit_material h) r h
                 case mscatter of
                   Just (sray, Attenuation att) ->
                     rayColorHelp
                       sray
                       (depth - 1)
                       (Attenuation (att_acc `vecMul` att))
                   Nothing -> return $ Vec3 (0.0, 0.0, 0.0)
               Nothing ->
                 let unitDirection = makeUnitVector (direction r)
                     t = 0.5 * (vecY unitDirection + 1.0)
                  in return $
                     att_acc `vecMul`
                     (scale (1.0 - t) (Vec3 (1.0, 1.0, 1.0)) `vecAdd`
                      scale t (Vec3 (0.5, 0.7, 1.0)))

sampleColor :: (Int, Int) -> Vec3 -> Int -> RayTracingM s Vec3
sampleColor (x, y) accCol _ = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (ru, g1) = randomDouble gen
  let (rv, g2) = randomDouble g1
  let u = (fromIntegral x + ru) / fromIntegral imageWidth
  let v = (fromIntegral y + rv) / fromIntegral imageHeight
  r <- getRay defaultCamera u v
  lift $ writeSTRef gRef g2
  c1 <- rayColor r maxDepth
  return $ accCol `vecAdd` c1

renderPos :: (Int, Int) -> RayTracingM s XYZ
renderPos (x, y) = do
  summedColor <-
    foldM (sampleColor (x, y)) (Vec3 (0.0, 0.0, 0.0)) [0 .. nsPerThread - 1]
  return $ divide summedColor (fromIntegral ns)

renderRow :: [(Int, Int)] -> RayTracingM s [Vec3]
renderRow = mapM renderPos

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  let pp = pixelPositions imageWidth imageHeight
  gen <- newPureMT
  -- let gen = pureMT 1024 -- Fix a seed for comparable performance tests
  let (world, g1) = makeWorld gen
  let vals =
        runST $ do
          worldRef <- newSTRef world
          gs <- mapM newSTRef (makeNPureMT g1 nThreads)
          gRef <- newSTRef g1
          mapM
            (\rowPs ->
               map scaleColors <$>
               parallelRenderRow rowPs worldRef gs)
            pp
  mapM_ printRow (zip [1 .. imageHeight] vals)
  hPutStr stderr "\nDone.\n"

makeNPureMT :: PureMT -> Int -> [PureMT]
makeNPureMT gen n =
  let (_, ps) =
        foldr
          (\_ (g, gs) ->
             let (newSeed, g1) = randomWord64 g
              in (g1, pureMT newSeed : gs))
          (gen, [])
          [1 .. n]
   in ps

parallelRenderRow ::
     [(Int, Int)] -> STRef s World -> [STRef s PureMT] -> ST s [Vec3]
parallelRenderRow rowps worldRef =
  foldM
    (\acc genRef -> do
       newRow <- runReaderT (renderRow rowps) (worldRef, genRef)
       return $ zipWith vecAdd acc newRow)
    (replicate imageWidth (Vec3 (0.0, 0.0, 0.0)))

-- |Generate the image from the cover of the book with lots of spheres
makeWorld :: PureMT -> (World, PureMT)
makeWorld gen =
  runST $ do
    gRef <- newSTRef gen
    worldRef <- newSTRef []
    world <- runReaderT makeWorldM (worldRef, gRef)
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
      if Lib.length (center `vecSub` Vec3 (4.0, 0.2, 0)) <= 0.9
        then return Nothing
        else if | mat < 0.8 -- Diffuse
                 ->
                  do a1 <- randomVec3DoubleM
                     a2 <- randomVec3DoubleM
                     let albedo = a1 `vecMul` a2
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
