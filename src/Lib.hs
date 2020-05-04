{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import           Control.Applicative           ((<$>))
import           Control.DeepSeq               (NFData, force, rnf)
import           Control.Monad                 (foldM)
import           Control.Monad.Reader
import           Control.Monad.ST.Lazy         (ST (..), runST, strictToLazyST)
import           Control.Monad.Trans           (lift)
import           Control.Parallel
import           Control.Parallel.Strategies   (Eval, parMap, rpar)
import           Data.Bits                     (xor, (.&.))
import           Data.Foldable                 (foldl')
import           Data.List                     (intercalate)
import           Data.Maybe                    (catMaybes)
import           Data.Sequence                 (Seq (..))
import qualified Data.Sequence                 as S
import           Data.STRef.Lazy
import qualified Data.Vector                   as VV (Vector (..), fromList,
                                                      (!))
import           Data.Vector.Unboxed           (Unbox (..), Vector (..),
                                                enumFromN, freeze, thaw)
import qualified Data.Vector.Unboxed           as V ((!))
import           Data.Vector.Unboxed.Mutable   (MVector (..))
import qualified Data.Vector.Unboxed.Mutable   as MV (new, swap, write)
import           Data.Word                     (Word8)
import           System.IO                     (hPutStr, stderr)
import           System.Random.Mersenne.Pure64

-- Number of threads to use when rendering
nThreads :: Int
nThreads = 2

-- Number of samples to use when anti-aliasing
ns :: Int
ns = 100

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

-- A scene should be a BVHNode, which is a Hittable
type Scene = Hittable

-- Use the ST monad to thread the random number generator
type RandomState s = ReaderT (STRef s Scene, Camera, STRef s PureMT) (ST s)

getGenRef :: RandomState s (STRef s PureMT)
getGenRef = do (_, _, genRef) <- ask
               return genRef

getCamera :: RandomState s Camera
getCamera = do (_, cam, _) <- ask
               return cam

getSceneRef :: RandomState s (STRef s Scene)
getSceneRef = do (worldRef, _, _) <- ask
                 return worldRef

instance NFData PureMT where
  rnf x = seq x ()

type RayTracingM s = RandomState s

-- Final representation of a color of a pixel before output
newtype RGB = RGB (Word8, Word8, Word8)

instance Show RGB where
  show (RGB (r, g, b)) = unwords [show r, show g, show b]

-- General 3-dimensional Doubles--could be color or vector or position
type XYZ = Vec3

newtype Vec3 = Vec3 (Double, Double, Double)

instance NFData Vec3 where
  rnf (Vec3 (x, y, z)) = rnf x `seq` rnf y `seq` rnf z

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

scaleColors :: Albedo -> RGB
scaleColors (Albedo (Vec3 (x, y, z))) =
  RGB (scaleColor x, scaleColor y, scaleColor z)

printRow :: (Int, [RGB]) -> IO ()
printRow (i, row) = do
  hPutStr stderr ("\rRendering row " ++ show i ++ " of " ++ show imageHeight)
  putStrLn $ showRow row

showRow :: [RGB] -> String
showRow row = unwords $ fmap show row


data Ray = Ray
  { ray_origin    :: XYZ
  , ray_direction :: XYZ
  , ray_time      :: Double
  } deriving Show

at :: Ray -> Double -> XYZ
at (Ray or dr _) t = or `vecAdd` scale t dr

data Hit
  = Hit { hit_t         :: Double
        , hit_p         :: XYZ
        , hit_normal    :: XYZ -- vector normal to the surface of the object
                         -- at the point of the hit
        , hit_u         :: Double
        , hit_v         :: Double
        , hit_frontFace :: Bool -- did the ray hit the outer face of the
                          -- object?
        , hit_material  :: Material }
  | EmptyHit

data Material
  = Lambertian Texture
  | Metal Texture Fuzz
  | Dielectric RefractiveIdx
  deriving Show

newtype Fuzz = Fuzz Double
  deriving Show

newtype RefractiveIdx = RefractiveIdx Double
  deriving Show

newtype Albedo = Albedo Vec3
  deriving Show

instance NFData Albedo where
  rnf (Albedo v) = rnf v `seq` ()

data Texture
  = ConstantColor { constColor :: Albedo }
  | CheckerTexture { checkerTextureOdd  :: Texture
                   , checkerTextureEven :: Texture }
  | Perlin { perlinRanFloat :: VV.Vector Vec3
           , perlinPermX    :: Vector Int
           , perlinPermY    :: Vector Int
           , perlinPermZ    :: Vector Int
           , perlinScale    :: Double
           }
  deriving Show

pointCount :: Int
pointCount = 256

makePerlin :: Double -> RandomState s Texture
makePerlin sc = do
  rds <- replicateM pointCount (randomVec3DoubleRM (-1.0) 1.0)
  Perlin (VV.fromList rds) <$> perlinGeneratePerm <*> perlinGeneratePerm <*>
    perlinGeneratePerm <*> return sc

perlinGeneratePerm :: RandomState s (Vector Int)
perlinGeneratePerm = do
  p <- lift $ strictToLazyST $ thaw $ enumFromN 0 pointCount
  foldM_
    (\mv i -> do
       target <- randomIntRM 0 i
       lift $ strictToLazyST $ MV.swap p i target)
    ()
    [pointCount - 1,pointCount - 2 .. 1]
  lift $ strictToLazyST $ freeze p

noise :: Texture -> Vec3 -> Double
noise (Perlin ranvec permX permY permZ sc) p =
  let p' = scale sc p
      i = floor (vecX p')
      j = floor (vecY p')
      k = floor (vecZ p')
      u = vecX p' - fromIntegral (floor $ vecX p')
      v = vecY p' - fromIntegral (floor $ vecY p')
      w = vecZ p' - fromIntegral (floor $ vecZ p')
      ds = [(di, dj, dk) | di <- [0, 1], dj <- [0, 1], dk <- [0, 1]]
      rf (di, dj, dk) =
        ranvec VV.!
        (permX V.! ((i + di) `mod` pointCount) `xor`
         permY V.! ((j + dj) `mod` pointCount) `xor`
         permZ V.! ((k + dk) `mod` pointCount))
      c =
        map
          (\d@(di, dj, dk) ->
             ((fromIntegral di, fromIntegral dj, fromIntegral dk), rf d))
          ds
  in perlinInterp c u v w

perlinInterp ::
     [((Double, Double, Double), Vec3)]
  -> Double
  -> Double
  -> Double
  -> Double
perlinInterp c u v w =
  let hermite z = z * z * (3 - 2 * z)
      uu = hermite u
      vv = hermite v
      ww = hermite w
   in foldr
        (\((i, j, k), val) acc ->
           acc +
           ((i * uu + (1 - i) * (1 - uu)) * (j * vv + (1 - j) * (1 - vv)) *
            (k * ww + (1 - k) * (1 - ww)) *
            (val `dot` Vec3 (u - i, v - j, w - k))))
        0.0
        c

turb :: Texture -> Vec3 -> Int -> Double
turb ptex p depth =
  let (accum, _, _) =
        foldr
          (\_ (acc, tempP, weight) ->
             (acc + weight * noise ptex tempP, scale 2.0 tempP, weight * 0.5))
          (0.0, p, 1.0)
          [1 .. depth]
   in abs accum

textureValue :: Texture -> Double -> Double -> Vec3 -> Albedo
textureValue (ConstantColor color) _ _ _ = color
textureValue (CheckerTexture oddTex evenTex) u v p =
  if sin (10 * vecX p) * sin (10 * vecY p) * sin (10 * vecZ p) < 0
  then textureValue oddTex u v p
  else textureValue evenTex u v p
textureValue ptex@Perlin {} u v p =
  Albedo $
  scale (0.5 * (1.0 + sin (vecZ p + 10 * turb ptex p 7))) $
  Vec3 (1.0, 1.0, 1.0)

data Hittable
  = Sphere { sphere_center   :: Vec3
           , sphere_radius   :: Double
           , sphere_material :: Material }
  | MovingSphere { msphere_center0  :: Vec3
                 , msphere_center1  :: Vec3
                 , msphere_time0    :: Time
                 , msphere_time1    :: Time
                 , msphere_radius   :: Double
                 , msphere_material :: Material }
  | Aabb { aabb_min :: Vec3
         , aabb_max :: Vec3 }
  | BVHNode {bvh_left   :: Hittable
            , bvh_right :: Hittable
            , bvh_box   :: Box}
    deriving Show

data Box = Box
  { box_min :: Vec3
  , box_max :: Vec3
  } deriving (Show)

boxToAabb :: Box -> Hittable
boxToAabb (Box bmin bmax) = Aabb bmin bmax

type Time = Double

sphCenter :: Hittable -> Double -> Vec3
sphCenter (Sphere c r _) _ = c
sphCenter (MovingSphere c0 c1 t0 t1 r m) t =
  c0 `vecAdd` scale ((t - t0) / (t1 - t0)) (c1 `vecSub` c0)

sphRadius :: Hittable -> Double
sphRadius (Sphere _ r _)             = r
sphRadius (MovingSphere _ _ _ _ r _) = r

sphMaterial :: Hittable -> Material
sphMaterial (Sphere _ _ m)             = m
sphMaterial (MovingSphere _ _ _ _ _ m) = m

scatter :: Material -> Ray -> Hit -> RandomState s (Maybe (Ray, Albedo))
scatter (Lambertian tex) rin (Hit _ hp hn hu hv _ _) = do
  rUnit <- randomUnitVectorM
  let scatterDirection = hn `vecAdd` rUnit
  let scattered = Ray hp scatterDirection (ray_time rin)
  return $ Just (scattered, textureValue tex hu hv hp)
scatter (Metal tex (Fuzz fuzz)) rin (Hit _ hp hn hu hv _ _) = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector (ray_direction rin)) hn
  let scattered = Ray hp (reflected `vecAdd` scale fuzz rUnit) (ray_time rin)
  return $ if dot (ray_direction scattered) hn > 0.0
           then Just (scattered, textureValue tex hu hv hp)
           else Nothing
scatter (Dielectric (RefractiveIdx ref_idx)) rin (Hit _ hp hn hu hv hff _) = do
  let albedo = Albedo $ Vec3 (1.0, 1.0, 1.0)
  let etaiOverEtat =
        if hff
          then 1.0 / ref_idx
          else ref_idx
  let unitDirection = makeUnitVector (ray_direction rin)
  let cosTheta = min (dot (vecNegate unitDirection) hn) 1.0
  let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
  rd <- randomDoubleM
  return $
    if (etaiOverEtat * sinTheta > 1.0) || rd < schlick cosTheta etaiOverEtat
      then let reflected = reflect unitDirection hn
            in Just (Ray hp reflected (ray_time rin), albedo)
      else let refracted = refract unitDirection hn etaiOverEtat
            in Just (Ray hp refracted (ray_time rin), albedo)

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

boundingBox :: Hittable -> Double -> Double -> Box
boundingBox (Sphere c r _) _ _ =
  let rad = Vec3 (r, r, r)
   in Box (c `vecSub` rad) (c `vecAdd` rad)
boundingBox (MovingSphere c0 c1 ms_t0 ms_t1 r _) t0 t1 =
  let rad = Vec3 (r, r, r)
      box0 = Box (c0 `vecSub` rad) (c0 `vecAdd` rad)
      box1 = Box (c1 `vecSub` rad) (c1 `vecAdd` rad)
   in surroundingBox box0 box1
boundingBox (Aabb ab_min ab_max) _ _ = Box ab_min ab_max
boundingBox (BVHNode _ _ box) _ _ = box

surroundingBox :: Box -> Box -> Box
surroundingBox (Box b0min b0max) (Box b1min b1max) =
  let (Vec3 (b0min_x, b0min_y, b0min_z)) = b0min
      (Vec3 (b0max_x, b0max_y, b0max_z)) = b0max
      (Vec3 (b1min_x, b1min_y, b1min_z)) = b1min
      (Vec3 (b1max_x, b1max_y, b1max_z)) = b1max
      small =
        Vec3 (min b0min_x b1min_x, min b0min_y b1min_y, min b0min_z b1min_z)
      big = Vec3 (max b0max_x b1max_x, max b0max_y b1max_y, max b0max_z b1max_z)
   in Box small big

makeBVH :: Time -> Time -> Seq Hittable -> RandomState s Hittable
makeBVH t0 t1 htbls = do
  axis <- floor <$> randomDoubleRM 0 3
  let comparator = boxCompare ([vecX, vecY, vecZ] !! axis) t0 t1
  let objectSpan = S.length htbls
  (bvh_lt, bvh_rt) <-
    case htbls of
      h :<| Empty -> return (h, h)
      h1 :<| h2 :<| Empty ->
        case comparator h1 h2 of
          LT -> return (h1, h2)
          _  -> return (h2, h1)
      _ -> do
        let (lt_htbls, rt_htbls) =
              S.splitAt (objectSpan `div` 2) (S.sortBy comparator htbls)
        lt <- makeBVH t0 t1 lt_htbls
        rt <- makeBVH t0 t1 rt_htbls
        return (lt, rt)
  let bvh_bx =
        surroundingBox (boundingBox bvh_lt t0 t1) (boundingBox bvh_rt t0 t1)
  return (BVHNode bvh_lt bvh_rt bvh_bx)

boxCompare ::
     (Vec3 -> Double) -> Time -> Time -> Hittable -> Hittable -> Ordering
boxCompare compAxis t0 t1 boxa boxb =
  compare
    (compAxis (box_min (boundingBox boxa t0 t1)))
    (compAxis (box_min (boundingBox boxb t0 t1)))

hit :: Hittable -> Ray -> Double -> Double -> Maybe Hit
hit (BVHNode bvh_l bvh_r box) r@(Ray or dr tm) t_min t_max =
  case hit (boxToAabb box) r t_min t_max of
    Nothing -> Nothing
    Just _ ->
      case hit bvh_l r t_min t_max of -- try to hit left branch
        Nothing -> hit bvh_r r t_min t_max -- no hits, try right branch
        Just hitLeft@(Hit t _ _ _ _ _ _) -> -- left branch hit
          case hit bvh_r r t_min t of -- is there a closer right branch hit?
            Nothing       -> Just hitLeft -- no, take hit from left branch
            Just hitRight -> Just hitRight -- yes, take hit from right branch
hit (Aabb bb_min bb_max) r@(Ray or dr tm) t_min t_max =
  foldr
    (\cur_axis mh ->
       case mh of
         Nothing -> Nothing
         _ ->
           let c_or = cur_axis or
               c_dr = cur_axis dr
               c_bb_min = cur_axis bb_min
               c_bb_max = cur_axis bb_max
               t0 = min ((c_bb_min - c_or) / c_dr) ((c_bb_max - c_or) / c_dr)
               t1 = max ((c_bb_min - c_or) / c_dr) ((c_bb_max - c_or) / c_dr)
               tmin = max t0 t_min
               tmax = min t1 t_max
            in if tmax <= tmin
                 then Nothing
                 else mh)
    (Just EmptyHit)
    [vecX, vecY, vecZ]
hit sphere r@(Ray or dr tm) t_min t_max =
  let sc = sphCenter sphere tm
      sr = sphRadius sphere
      oc = or `vecSub` sc
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

recHit :: Double -> Ray -> Hittable -> Hit
recHit temp r@(Ray or dr tm) sphere =
  let sc = sphCenter sphere tm
      sr = sphRadius sphere
      sm = sphMaterial sphere
      p = r `at` temp
      pShift = p `vecSub` sc
      outwardNormal = divide pShift sr
      frontFace = dot dr outwardNormal < 0.0
      n =
        if frontFace
          then outwardNormal
          else vecNegate outwardNormal
      (u, v) = let phi = atan2 (vecZ outwardNormal) (vecX outwardNormal)
                   theta = asin (vecY outwardNormal)
        in (1.0 - ((phi + pi) / (2 * pi)), (theta + (pi / 2)) / pi)
   in Hit temp p n u v frontFace sm

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

randomIntRM :: Int -> Int -> RandomState s Int
randomIntRM lo hi = floor <$> randomDoubleRM (fromIntegral lo) (fromIntegral hi)

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
  , camera_t0         :: Double
  , camera_t1         :: Double
  }

getRay :: Camera -> Double -> Double -> RandomState s Ray
getRay c s t = do
  rd <- fmap (scale $ camera_lensRadius c) randomInUnitDiskM
  let offset =
        scale (vecX rd) (camera_u c) `vecAdd` scale (vecY rd) (camera_v c)
  tm <- randomDoubleRM (camera_t0 c) (camera_t1 c)
  return $
    Ray
      (camera_origin c `vecAdd` offset)
      (camera_llc c `vecAdd` scale s (camera_horiz c) `vecAdd`
       scale t (camera_vert c) `vecSub`
       camera_origin c `vecSub`
       offset)
      tm

newCamera ::
     XYZ
  -> XYZ
  -> XYZ
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Camera
newCamera lookfrom lookat vup vfov aspect aperture focusDist t0 t1 =
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
   in Camera origin lowerLeftCorner horizontal vertical u v w lensRadius t0 t1

rayColor :: Ray -> Int -> RayTracingM s Albedo
rayColor r depth = rayColorHelp r depth (Albedo $ Vec3 (1.0, 1.0, 1.0))
  where
    rayColorHelp :: Ray -> Int -> Albedo -> RayTracingM s Albedo
    rayColorHelp r depth (Albedo alb_acc) = do
      worldRef <- getSceneRef
      htbls <- lift $ readSTRef worldRef
      if depth <= 0
        then return $ Albedo $ Vec3 (0.0, 0.0, 0.0)
        else case hit htbls r 0.001 infinity of
               Just h -> do
                 mscatter <- scatter (hit_material h) r h
                 case mscatter of
                   Just (sray, Albedo alb) ->
                     rayColorHelp
                       sray
                       (depth - 1)
                       (Albedo $ alb_acc `vecMul` alb)
                   Nothing -> return $ Albedo $ Vec3 (0.0, 0.0, 0.0)
               Nothing ->
                 let unitDirection = makeUnitVector (ray_direction r)
                     t = 0.5 * (vecY unitDirection + 1.0)
                  in return $ Albedo $
                     alb_acc `vecMul`
                     (scale (1.0 - t) (Vec3 (1.0, 1.0, 1.0)) `vecAdd`
                      scale t (Vec3 (0.5, 0.7, 1.0)))

sampleColor :: (Int, Int) -> Albedo -> Int -> RayTracingM s Albedo
sampleColor (x, y) (Albedo accCol) _ = do
  gRef <- getGenRef
  gen <- lift $ readSTRef gRef
  let (ru, g1) = randomDouble gen
  let (rv, g2) = randomDouble g1
  let u = (fromIntegral x + ru) / fromIntegral imageWidth
  let v = (fromIntegral y + rv) / fromIntegral imageHeight
  camera <- getCamera
  r <- getRay camera u v
  lift $ writeSTRef gRef g2
  (Albedo c1) <- rayColor r maxDepth
  return $ Albedo $ accCol `vecAdd` c1

renderPos :: (Int, Int) -> RayTracingM s Albedo
renderPos (x, y) = do
  (Albedo summedColor) <-
    foldM
      (sampleColor (x, y))
      (Albedo $ Vec3 (0.0, 0.0, 0.0))
      [0 .. nsPerThread - 1]
  return $ Albedo $ divide summedColor (fromIntegral ns)

renderRow :: [(Int, Int)] -> RayTracingM s [Albedo]
renderRow = mapM renderPos

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

someFunc :: IO ()
someFunc = do
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  let pp = pixelPositions imageWidth imageHeight
  --gen <- newPureMT
  let gen = pureMT 1024 -- Fix a seed for comparable performance tests
  --let (world, g1) = makeRandomScene 0.0 1.0 gen
  let (world, g1) = makeTwoPerlinSpheresScene 0.0 1.0 gen
  let camera = randomSceneCamera
  gs <- replicateM (nThreads - 1) newPureMT
  let gens = g1 : gs
  let vals =
        runST $ do
        gensRef <- newSTRef gens
        mapM
          (\rowPs ->
             map scaleColors <$>
             parallelRenderRow rowPs world camera gensRef)
          pp
  mapM_ printRow (zip [1 .. imageHeight] vals)
  hPutStr stderr "\nDone.\n"

parallelRenderRow ::
     [(Int, Int)] -> Scene -> Camera -> STRef s [PureMT] -> ST s [Albedo]
parallelRenderRow rowps world camera gsRef = do
  gs <- readSTRef gsRef
  let (sampleGroups, newGs) =
        unzip $
        parMap
          rpar
          (\gen ->
             force $
             runST $ do
               worldRef <- newSTRef world
               genRef <- newSTRef gen
               runReaderT
                 (do row <- renderRow rowps
                     g <- lift $ readSTRef genRef
                     return (row, g))
                 (worldRef, camera, genRef))
          gs
  writeSTRef gsRef newGs
  return $
    foldl'
      (zipWith (\(Albedo a1) (Albedo a2) -> Albedo $ vecAdd a1 a2))
      (replicate imageWidth (Albedo $ Vec3 (0.0, 0.0, 0.0)))
      sampleGroups

twoSpheresSceneCamera :: Camera
twoSpheresSceneCamera =
  newCamera
    (Vec3 (13.0, 2.0, 3.0))
    (Vec3 (0.0, 0.0, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    10.0
    0.0
    1.0

makeTwoPerlinSpheresScene :: Time -> Time -> PureMT -> (Scene, PureMT)
makeTwoPerlinSpheresScene t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    worldRef <- newSTRef (Aabb (Vec3 (0, 0, 0)) (Vec3 (0, 0, 0)))
    world <-
      runReaderT
        (do perText <- makePerlin 1.5
            makeBVH
              t0
              t1
              (Sphere (Vec3 (0, -1000, 0)) 1000 (Lambertian perText) :<|
               Sphere (Vec3 (0, 2, 0)) 2 (Lambertian perText) :<|
               Empty))
        (worldRef, twoSpheresSceneCamera, gRef)
    g1 <- readSTRef gRef
    return (world, g1)

makeTwoSpheresScene :: Time -> Time -> PureMT -> (Scene, PureMT)
makeTwoSpheresScene t0 t1 gen =
  runST $ do
    let checkerMaterial =
          Metal
            (CheckerTexture
               (ConstantColor $ Albedo $ Vec3 (0.2, 0.3, 0.1))
               (ConstantColor $ Albedo $ Vec3 (0.9, 0.9, 0.9)))
            (Fuzz 0.0)
    let flatMaterial =
          Lambertian (ConstantColor $ Albedo $ Vec3 (0.6, 0.2, 0.1))
    gRef <- newSTRef gen
    worldRef <- newSTRef (Aabb (Vec3 (0, 0, 0)) (Vec3 (0, 0, 0)))
    world <-
      runReaderT
        (makeBVH
           t0
           t1
           (Sphere (Vec3 (0, -10, 0)) 10 checkerMaterial :<|
            Sphere (Vec3 (0, 10, 0)) 10 flatMaterial :<|
            Empty))
        (worldRef, twoSpheresSceneCamera, gRef)
    g1 <- readSTRef gRef
    return (world, g1)

randomSceneCamera :: Camera
randomSceneCamera =
  newCamera
    (Vec3 (13.0, 2.0, 3.0))
    (Vec3 (0.0, 0.0, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    10.0
    0.0
    1.0

-- |Generate the image from the cover of the book with lots of spheres
makeRandomScene :: Time -> Time -> PureMT -> (Scene, PureMT)
makeRandomScene t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    -- dummy world to satisfy RandomState:
    worldRef <- newSTRef (Aabb (Vec3 (0, 0, 0)) (Vec3 (0, 0, 0)))
    world <- runReaderT makeSceneM (worldRef, randomSceneCamera, gRef)
    g1 <- readSTRef gRef
    return (world, g1)
  where
    makeSceneM :: RandomState s Scene
    makeSceneM = do
      let ns = [(x, y) | x <- [-11 .. 10], y <- [-11 .. 10]]
      let ground =
            Sphere
              (Vec3 (0.0, -1000.0, 0.0))
              1000
              -- (Lambertian (ConstantColor $ Albedo $ Vec3 (0.5, 0.5, 0.5))) --gray
              (Lambertian
                 (CheckerTexture
                    (ConstantColor $ Albedo $ Vec3 (0.2, 0.3, 0.1))
                    (ConstantColor $ Albedo $ Vec3 (0.9, 0.9, 0.9))))
      let s1 =
            Sphere (Vec3 (0.0, 1.0, 0.0)) 1.0 (Dielectric (RefractiveIdx 1.5))
      let s2 =
            Sphere
              (Vec3 (-4.0, 1.0, 0.0))
              1.0
              (Lambertian (ConstantColor $ Albedo $ Vec3 (0.4, 0.2, 0.1)))
      let s3 =
            Sphere
              (Vec3 (4.0, 1.0, 0.0))
              1.0
              (Metal (ConstantColor $ Albedo $ Vec3 (0.7, 0.6, 0.5)) (Fuzz 0.0))
      nps <- catMaybes <$> mapM makeRandomSphereM ns
      makeBVH 0.0 1.0 $ ground :<| s1 :<| s2 :<| s3 :<| S.fromList nps
    makeRandomSphereM :: (Int, Int) -> RandomState s (Maybe Hittable)
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
                     sph_move_x <- randomDoubleRM (-0.25) 0.25
                     sph_move_z <- randomDoubleRM (-0.25) 0.25
                     let albedo = Albedo $ a1 `vecMul` a2
                     return $
                       Just $
                       MovingSphere
                         center
                         (center `vecAdd` Vec3 (sph_move_x, 0, sph_move_z))
                         0.0
                         1.0
                         0.2
                         (Lambertian (ConstantColor albedo))
                | mat < 0.95 -- Metal
                 ->
                  do albedo <- Albedo <$> randomVec3DoubleRM 0.5 1.0
                     fuzz <- randomDoubleRM 0.0 0.5
                     return $
                       Just $
                       Sphere
                         center
                         0.2
                         (Metal (ConstantColor albedo) (Fuzz fuzz))
                | otherwise --Glass
                 ->
                  return $
                  Just $ Sphere center 0.2 (Dielectric (RefractiveIdx 1.5))
