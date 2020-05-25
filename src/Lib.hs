{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( cornellCamera
    , earthTexture
    , makeCornellBoxScene
    , makeEarthScene
    , makeRandomScene
    , makeSimpleLightScene
    , makeTwoPerlinSpheresScene
    , makeTwoSpheresScene
    , mkRenderStaticEnv
    , newRandGen
    , printRow
    , randGen
    , randomSceneCamera
    , runRender
    , twoSpheresSceneCamera
    ) where

import qualified Codec.Picture                 as JP (Image (..),
                                                      PixelRGB8 (..),
                                                      convertRGB8, imageHeight,
                                                      imageWidth, pixelAt,
                                                      readImage)
import           Control.Applicative           ((<$>))
import           Control.DeepSeq               (NFData, force, rnf)
import           Control.Monad                 (foldM)
import           Control.Monad.Reader
import           Control.Monad.ST.Lazy         (ST, runST, strictToLazyST)
import           Control.Monad.Trans           (lift)
import           Control.Parallel.Strategies   (parMap, rpar)
import           Data.Bits                     (xor)
import           Data.Foldable                 (foldl')
import           Data.Maybe                    (catMaybes)
import           Data.Sequence                 (Seq (..))
import qualified Data.Sequence                 as S
import           Data.STRef.Lazy
import qualified Data.Vector                   as VV (Vector, fromList, (!))
import           Data.Vector.Unboxed           (Vector, enumFromN, freeze, thaw)
import qualified Data.Vector.Unboxed           as V ((!))
import qualified Data.Vector.Unboxed.Mutable   as MV (swap)
import           Data.Word                     (Word64, Word8)
import           System.IO                     (Handle, hPutStr, hPutStrLn,
                                                stderr)
import qualified System.Random.Mersenne.Pure64 as MT

-- Epsilon (some smallish number)
epsilon :: Double
epsilon = 0.0001

infinity :: Double
infinity = read "Infinity" :: Double

-- A scene should be a BVHNode, which is a Hittable, and backrgound color
type Scene = (Hittable, Albedo)

-- Use the ST monad to thread the random number generator
type RandomState s = ReaderT (RenderEnv s) (ST s)

newtype RenderStaticEnv =
  RenderStaticEnv (Scene, Camera, (Int, Int), Int, Int, Int, Int)

mkRenderStaticEnv ::
     Scene
  -> Camera
  -> (Int, Int)
  -> Int
  -> Int
  -> Int
  -> RenderStaticEnv
mkRenderStaticEnv s c (width, height) numSamples maxDepth numThreads =
  RenderStaticEnv
    ( s
    , c
    , (width, height)
    , numSamples
    , maxDepth
    , numThreads
    , numSamples `div` numThreads)

getStaticImageWidth :: RenderStaticEnv -> Int
getStaticImageWidth (RenderStaticEnv (_, _, (width, _), _, _, _, _)) = width

getStaticImageHeight :: RenderStaticEnv -> Int
getStaticImageHeight (RenderStaticEnv (_, _, (_, height), _, _, _, _)) =
  height

newtype RandGen = RandGen MT.PureMT
  deriving Show

newRandGen :: IO RandGen
newRandGen = RandGen <$> MT.newPureMT

randGen :: Word64 -> RandGen
randGen s = RandGen (MT.pureMT s)

newtype RenderEnv s =
  RenderEnv ( RenderStaticEnv
            , STRef s RandGen)

mkRenderEnv :: RenderStaticEnv -> STRef s RandGen -> RenderEnv s
mkRenderEnv renderStaticEnv g =
  RenderEnv
    ( renderStaticEnv
    , g)

dummyCamera :: Camera
dummyCamera =
  newCamera
    (Vec3 (13.0, 2.0, 3.0))
    (Vec3 (0.0, 0.0, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (4 / 3)
    0.1
    10.0
    0.0
    1.0

dummyRenderStaticEnv :: RenderStaticEnv
dummyRenderStaticEnv =
  let world =
        ( Sphere
            (Vec3 (0, 0, 0))
            1.0
            (Lambertian $ ConstantColor $ albedo (0, 1.0, 1.0))
        , albedo (0, 0, 0))
   in mkRenderStaticEnv world dummyCamera (0, 0) 0 0 0

dummyRenderEnv :: STRef s RandGen -> RenderEnv s
dummyRenderEnv = mkRenderEnv dummyRenderStaticEnv

getSceneHittables :: RenderEnv s -> Hittable
getSceneHittables (RenderEnv (RenderStaticEnv ((scn, _), _, _, _, _, _, _), _)) =
  scn

getBackground :: RenderEnv s -> Albedo
getBackground (RenderEnv (RenderStaticEnv ((_, bkgd), _, _, _, _, _, _), _)) =
  bkgd

getCamera :: RenderEnv s -> Camera
getCamera (RenderEnv (RenderStaticEnv (_, cam, _, _, _, _, _), _)) = cam

getImageWidth :: RenderEnv s -> Int
getImageWidth (RenderEnv (staticEnv, _)) = getStaticImageWidth staticEnv

getImageHeight :: RenderEnv s -> Int
getImageHeight (RenderEnv (staticEnv, _)) = getStaticImageHeight staticEnv

getNumSamples :: RenderEnv s -> Int
getNumSamples (RenderEnv (RenderStaticEnv (_, _, _, numSamples, _, _, _), _)) =
  numSamples

getMaxDepth :: RenderEnv s -> Int
getMaxDepth (RenderEnv (RenderStaticEnv (_, _, _, _, maxDepth, _, _), _)) =
  maxDepth

getNsPerThread :: RenderEnv s -> Int
getNsPerThread (RenderEnv (RenderStaticEnv (_, _, _, _, _, _, nsPerThread), _))
  = nsPerThread

getGenRef :: RenderEnv s -> STRef s RandGen
getGenRef (RenderEnv (_, genRef)) = genRef

instance NFData RandGen where
  rnf (RandGen pmt) = seq pmt ()

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

_vecDiv :: Vec3 -> Vec3 -> Vec3
_vecDiv (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
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

clamp :: (Double, Double) -> Double -> Double
clamp (mn, mx) x =
  if | x < mn -> mn
     | x > mx -> mx
     | otherwise -> x

scaleColor :: Double -> Word8
scaleColor x = floor $ 256 * clamp (0.0, 0.999) (sqrt x)

albedoToColor :: Albedo -> RGB
albedoToColor (Albedo (Vec3 (x, y, z))) =
  RGB (scaleColor x, scaleColor y, scaleColor z)

colorToAlbedo :: RGB -> Albedo
colorToAlbedo (RGB (r, g, b)) =
  Albedo
    (Vec3
       (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0))

printRow :: Handle -> Int -> (Int, [RGB]) -> IO ()
printRow handle imageHeight (i, row) = do
  hPutStr stderr ("\rRendering row " ++ show i ++ " of " ++ show imageHeight)
  hPutStrLn handle $ showRow row

showRow :: [RGB] -> String
showRow row = unwords $ fmap show row

data Ray =
  Ray XYZ -- | ray_origin
      XYZ -- | ray_direction
      Double -- | ray_time
  deriving (Show)

at :: Ray -> Double -> XYZ
at (Ray orn dr _) t = orn `vecAdd` scale t dr

data Hit
  = Hit Double   -- | hit_t
        XYZ      -- | hit_p
        XYZ      -- | hit_normal - vector normal to the surface of the object
                 -- at the point of the hit
        Double   -- | hit_u
        Double   -- | hit_v
        Bool     -- | hit_frontFace --  did the ray hit the outer face of the
                 -- object?                   -
        Material -- | hit_material}

data Material
  = Lambertian Texture
  | Metal Texture Fuzz
  | Dielectric RefractiveIdx
  | DiffuseLight Texture
  | Isotropic Texture
  deriving Show

newtype Fuzz = Fuzz Double
  deriving Show

newtype RefractiveIdx = RefractiveIdx Double
  deriving Show

newtype Albedo = Albedo Vec3
  deriving Show

albedo :: (Double, Double, Double) -> Albedo
albedo rgb = Albedo $ Vec3 rgb

instance NFData Albedo where
  rnf (Albedo v) = rnf v `seq` ()

newtype Image = Image (JP.Image JP.PixelRGB8)

pixelAt :: Image -> Int -> Int -> RGB
pixelAt (Image im) i j =
  let (JP.PixelRGB8 r g b) = JP.pixelAt im i j
   in RGB (r, g, b)

instance Show Image where
  show _ = "<Image>"

data Texture
  = ConstantColor Albedo
  | CheckerTexture Texture -- | checkerTextureOdd
                   Texture -- | checkerTextureEven
  | Perlin (VV.Vector Vec3) -- | perlinRanFloat
           (Vector Int) -- | perlinPermX
           (Vector Int) -- | perlinPermY
           (Vector Int) -- | perlinPermZ
           Double -- | perlinScale
  | ImageTexture (Maybe Image) -- | imageTexture_image
                 Int -- | imageTexture_width
                 Int -- | imageTexture_height
  deriving (Show)

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
    (\_ i -> do
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
      u = vecX p' - fromIntegral (floor $ vecX p' :: Int)
      v = vecY p' - fromIntegral (floor $ vecY p' :: Int)
      w = vecZ p' - fromIntegral (floor $ vecZ p' :: Int)
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
noise (ConstantColor _) _ = undefined
noise (CheckerTexture _ _) _ = undefined
noise ImageTexture {} _ = undefined

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
textureValue ptex@Perlin{} _ _ p =
  Albedo $ scale (marbleTexture ptex p) $ Vec3 (1.0, 1.0, 1.0)
textureValue (ImageTexture (Just im) nx ny) u v _ =
  let nxd = fromIntegral nx
      i = floor $ clamp (0, nxd - epsilon) (u * nxd)
      nyd = fromIntegral ny
      j = floor $ clamp (0, nyd - epsilon) ((1.0 - v) * nyd - epsilon)
   in colorToAlbedo $ pixelAt im i j
textureValue (ImageTexture Nothing _ _) _ _ _ = albedo (0, 1, 1)

marbleTexture :: Texture -> Vec3 -> Double
marbleTexture ptex p = 0.5 * (1.0 + sin (vecZ p + 10 * turb ptex p 7))

data Axis = XAxis | YAxis | ZAxis
  deriving Show

data Plane = XYPlane | XZPlane | YZPlane
  deriving Show

data Hittable
  = Sphere Vec3     -- | sphere_center
           Double   -- | sphere_radius
           Material -- | sphere_material
  | MovingSphere Vec3     -- | msphere_center0
                 Vec3     -- | msphere_center1
                 Time     -- | msphere_time0
                 Time     -- | msphere_time1
                 Double   -- | msphere_radius
                 Material -- | msphere_material
  | Rect Rectangle
  | Cuboid Vec3 -- | box min
           Vec3 -- | box max
           [Rectangle] -- | Rects of the six sides of the box
  | BVHNode Hittable -- | bvh_left
            Hittable -- | bvh_right
            Box      -- | bvh_box
  | Translate Vec3      -- | translation offset
              Hittable  -- | the translated Hittable
  | Rotate Axis     -- | the axis of rotation
           Double   -- | sin theta
           Double   -- | cos theta
           Box      -- | the BoundingBox
           Hittable -- | the rotated Hittable
  | ConstantMedium Double   -- | negative inverse density of the constant medium
                   Material -- | material of the constant medium
                   Hittable -- | the shape of the constant medium
  deriving (Show)

sphere :: Vec3 -> Double -> Material -> Hittable
sphere = Sphere

movingSphere :: Vec3 -> Vec3 -> Time -> Time -> Double -> Material -> Hittable
movingSphere = MovingSphere

cuboid :: Vec3 -> Vec3 -> Material -> Hittable
cuboid bmin@(Vec3 (x0, y0, z0)) bmax@(Vec3 (x1, y1, z1)) mat = Cuboid
  bmin
  bmax
  [ XYRect x0 x1 y0 y1 z1 mat
  , XYRect x0 x1 y0 y1 z0 mat
  , XZRect x0 x1 z0 z1 y1 mat
  , XZRect x0 x1 z0 z1 y0 mat
  , YZRect y0 y1 z0 z1 x1 mat
  , YZRect y0 y1 z0 z1 x0 mat
  ]

data Rectangle
  = XYRect Double   -- | xyrect_x0
           Double   -- | xyrect_x1
           Double   -- | xyrect_y0
           Double   -- | xyrect_y1
           Double   -- | xyrect_k
           Material -- | xyrect_material
  | XZRect Double   -- | _xzrect_x0
           Double   -- | _xzrect_x1
           Double   -- | _xzrect_z0
           Double   -- | _xzrect_z1
           Double   -- | _xzrect_k
           Material -- | _xzrect_material
  | YZRect Double   -- | _yzrect_y0
           Double   -- | _yzrect_y1
           Double   -- | _yzrect_z0
           Double   -- | _yzrect_z1
           Double   -- | _yzrect_k
           Material -- | _yzrect_material
    deriving Show

rect ::
     Plane
  -> Double
  -> Double
  -> Double
  -> Double
  -> Double
  -> Material
  -> Hittable
rect XYPlane x0 x1 y0 y1 k mat = Rect $ XYRect x0 x1 y0 y1 k mat
rect XZPlane x0 x1 z0 z1 k mat = Rect $ XZRect x0 x1 z0 z1 k mat
rect YZPlane y0 y1 z0 z1 k mat = Rect $ YZRect y0 y1 z0 z1 k mat

translate :: XYZ -> Hittable -> Hittable
translate = Translate

degreesToRadians :: Double -> Double
degreesToRadians angle = angle * pi / 180.0

rotate :: Axis -> Double -> Hittable -> Hittable
rotate axis angle h = Rotate axis sin_theta cos_theta (Box h_min h_max) h
  where
    rad = degreesToRadians angle
    sin_theta = sin rad
    cos_theta = cos rad
    (Box (Vec3 (bbMinX, bbMinY, bbMinZ)) (Vec3 (bbMaxX, bbMaxY, bbMaxZ))) =
      boundingBox h Nothing
    updateMinMax :: XYZ -> (XYZ, XYZ) -> (XYZ, XYZ)
    updateMinMax (Vec3 (i, j, k)) (Vec3 (minX, minY, minZ), Vec3 (maxX, maxY, maxZ)) =
      let x = i * bbMaxX + (1 - i) * bbMinX
          y = j * bbMaxY + (1 - j) * bbMinY
          z = k * bbMaxZ + (1 - k) * bbMinZ
          Vec3 (newX, newY, newZ) =
            rotatePoint axis sin_theta cos_theta (Vec3 (x, y, z))
       in ( Vec3 (min newX minX, min newY minY, min newZ minZ)
          , Vec3 (max newX maxX, max newY maxY, max newZ maxZ))
    (h_min, h_max) =
      foldr
        updateMinMax
        ( Vec3 (infinity, infinity, infinity)
        , Vec3 (-infinity, -infinity, -infinity))
        ([Vec3 (i, j, k) | i <- [0, 1, 2], j <- [0, 1, 2], k <- [0, 1, 2]])

rotatePoint :: Axis -> Double -> Double -> XYZ -> XYZ
rotatePoint axis sin_theta cos_theta (Vec3 (pX, pY, pZ)) =
  case axis of
    XAxis ->
      Vec3
        (pX, cos_theta * pY - sin_theta * pZ, sin_theta * pY + cos_theta * pZ)
    YAxis ->
      Vec3
        (cos_theta * pX + sin_theta * pZ, pY, -sin_theta * pX + cos_theta * pZ)
    ZAxis ->
      Vec3
        (cos_theta * pX - sin_theta * pY, sin_theta * pX + cos_theta * pY, pZ)

unRotatePoint :: Axis -> Double -> Double -> XYZ -> XYZ
unRotatePoint axis sin_theta cos_theta (Vec3 (pX, pY, pZ)) =
  case axis of
    XAxis ->
      Vec3
        (pX, cos_theta * pY + sin_theta * pZ, -sin_theta * pY + cos_theta * pZ)
    YAxis ->
      Vec3
        (cos_theta * pX - sin_theta * pZ, pY, sin_theta * pX + cos_theta * pZ)
    ZAxis ->
      Vec3
        (cos_theta * pX + sin_theta * pY, -sin_theta * pX + cos_theta * pY, pZ)

constantMedium :: Double -> Texture -> Hittable -> Hittable
constantMedium density tex boundary =
  ConstantMedium (-1 / density) (Isotropic tex) boundary

data Box = Box
  { box_min :: Vec3
  , box_max :: Vec3
  } deriving (Show)

boxRayIntersect :: Box -> Ray -> Double -> Double -> Bool
boxRayIntersect (Box bb_min bb_max) (Ray ror rdr _) t_min t_max = all
  (\cur_axis ->
    let c_or     = cur_axis ror
        c_dr     = cur_axis rdr
        c_bb_min = cur_axis bb_min
        c_bb_max = cur_axis bb_max
        t0       = min ((c_bb_min - c_or) / c_dr) ((c_bb_max - c_or) / c_dr)
        t1       = max ((c_bb_min - c_or) / c_dr) ((c_bb_max - c_or) / c_dr)
        tmin     = max t0 t_min
        tmax     = min t1 t_max
    in  tmax > tmin
  )
  [vecX, vecY, vecZ]

type Time = Double

sphCenter :: Hittable -> Double -> Vec3
sphCenter (Sphere c _ _) _ = c
sphCenter (MovingSphere c0 c1 t0 t1 _ _) t =
  c0 `vecAdd` scale ((t - t0) / (t1 - t0)) (c1 `vecSub` c0)
sphCenter x _ = error $ "sphCenter called on non-sphere " ++ show x

sphRadius :: Hittable -> Double
sphRadius (Sphere _ r _)             = r
sphRadius (MovingSphere _ _ _ _ r _) = r
sphRadius x = error $ "sphRadius called on non-sphere " ++ show x

sphMaterial :: Hittable -> Material
sphMaterial (Sphere _ _ m)             = m
sphMaterial (MovingSphere _ _ _ _ _ m) = m
sphMaterial x = error $ "sphMaterial called on non-sphere " ++ show x

scatter :: Material -> Ray -> Hit -> RandomState s (Maybe (Ray, Albedo))
scatter (Lambertian tex) (Ray _ _ rtime) (Hit _ hp hn hu hv _ _) = do
  rUnit <- randomUnitVectorM
  let scatterDirection = hn `vecAdd` rUnit
  let scattered = Ray hp scatterDirection rtime
  return $ Just (scattered, textureValue tex hu hv hp)
scatter (Metal tex (Fuzz fuzz)) (Ray _ rdr rtime) (Hit _ hp hn hu hv _ _) = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector rdr) hn
  let scattered@(Ray _ scat_dir _) =
        Ray hp (reflected `vecAdd` scale fuzz rUnit) rtime
  return $
    if dot scat_dir hn > 0.0
      then Just (scattered, textureValue tex hu hv hp)
      else Nothing
scatter (Dielectric (RefractiveIdx ref_idx)) (Ray _ rdr rtime) (Hit _ hp hn _ _ hff _) = do
  let alb = albedo (1.0, 1.0, 1.0)
  let etaiOverEtat =
        if hff
          then 1.0 / ref_idx
          else ref_idx
  let unitDirection = makeUnitVector rdr
  let cosTheta = min (dot (vecNegate unitDirection) hn) 1.0
  let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
  rd <- randomDoubleM
  return $
    if (etaiOverEtat * sinTheta > 1.0) || rd < schlick cosTheta etaiOverEtat
      then let reflected = reflect unitDirection hn
            in Just (Ray hp reflected rtime, alb)
      else let refracted = refract unitDirection hn etaiOverEtat
            in Just (Ray hp refracted rtime, alb)
scatter DiffuseLight {} _ _  = return Nothing
scatter (Isotropic tex) (Ray _ _ rtime) (Hit _ hp _ hu hv _ _) = do
  randDr <- randomInUnitSphereM
  let scattered = Ray hp randDr rtime
  let attenuation = textureValue tex hu hv hp
  return $ Just (scattered, attenuation)

emitted :: Material -> Double -> Double -> Vec3 -> Albedo
emitted (DiffuseLight tex) u v p = textureValue tex u v p
emitted _ _ _ _                  = albedo (0, 0, 0)

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
schlick cosine ref_idx =
  let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
      r1 = r0 * r0
   in r1 + (1.0 - r1) * (1 - cosine) ** 5

boundingBox :: Hittable -> Maybe (Time, Time) -> Box
boundingBox (Sphere c r _) _ =
  let rad = Vec3 (r, r, r)
   in Box (c `vecSub` rad) (c `vecAdd` rad)
boundingBox (MovingSphere c0 c1 _ _ r _) _ =
  let rad = Vec3 (r, r, r)
      box0 = Box (c0 `vecSub` rad) (c0 `vecAdd` rad)
      box1 = Box (c1 `vecSub` rad) (c1 `vecAdd` rad)
   in surroundingBox box0 box1
boundingBox (Rect (XYRect x0 x1 y0 y1 k _)) _ =
  Box (Vec3 (x0, y0, k - epsilon)) (Vec3 (x1, y1, k + epsilon))
boundingBox (Rect (XZRect x0 x1 z0 z1 k _)) _ =
  Box (Vec3 (x0, k - epsilon, z0)) (Vec3 (x1, k + epsilon, z1))
boundingBox (Rect (YZRect y0 y1 z0 z1 k _)) _ =
  Box (Vec3 (k - epsilon, y0, z0)) (Vec3 (k + epsilon, y1, z1))
boundingBox (BVHNode _ _ box) _ = box
boundingBox (Cuboid c_min c_max _) _ = Box c_min c_max
boundingBox (Translate offset h) mtime =
  let (Box b_min b_max) = boundingBox h mtime
   in Box (b_min `vecAdd` offset) (b_max `vecAdd` offset)
boundingBox (Rotate _ _ _ box _) _ = box
boundingBox (ConstantMedium _ _ h) mt = boundingBox h mt

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

makeBVH :: Maybe (Time, Time) -> Seq Hittable -> RandomState s Hittable
makeBVH mtime htbls = do
  axis <- floor <$> randomDoubleRM 0 3
  let comparator = boxCompare ([vecX, vecY, vecZ] !! axis) mtime
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
        lt <- makeBVH mtime lt_htbls
        rt <- makeBVH mtime rt_htbls
        return (lt, rt)
  let bvh_bx =
        surroundingBox (boundingBox bvh_lt mtime) (boundingBox bvh_rt mtime)
  return (BVHNode bvh_lt bvh_rt bvh_bx)

boxCompare ::
     (Vec3 -> Double) -> Maybe (Time, Time) -> Hittable -> Hittable -> Ordering
boxCompare compAxis mtime boxa boxb =
  let (Box bmA _) = boundingBox boxa mtime
      (Box bmB _) = boundingBox boxb mtime
   in compare (compAxis bmA) (compAxis bmB)

hit :: Hittable -> Ray -> Double -> Double -> RandomState s (Maybe Hit)
hit (BVHNode bvh_l bvh_r box) r t_min t_max =
  if boxRayIntersect box r t_min t_max
    then do
      lefthit <- hit bvh_l r t_min t_max -- try to hit left branch
      case lefthit of
        Nothing -> hit bvh_r r t_min t_max -- no hits, try right branch
        Just hitLeft@(Hit t _ _ _ _ _ _) -- left branch hit
         -> do
          rightHit <- hit bvh_r r t_min t -- is there a closer right branch hit?
          return $
            case rightHit of
              Nothing       -> Just hitLeft -- no, take hit from left branch
              Just hitRight -> Just hitRight -- yes, take hit from right branch
    else return Nothing
hit (Cuboid _ _ rl) r t_min t_max = do
  maybeRectHits <- mapM (\h -> hit (Rect h) r t_min t_max) rl
  foldM closerHit Nothing maybeRectHits
  where
    closerHit :: Maybe Hit -> Maybe Hit -> RandomState s (Maybe Hit)
    closerHit Nothing Nothing = return Nothing
    closerHit h Nothing = return h
    closerHit Nothing h = return h
    closerHit h1@(Just (Hit t1 _ _ _ _ _ _)) h2@(Just (Hit t2 _ _ _ _ _ _)) =
      return $ if t1 < t2 then h1 else h2
hit (Rect rct) r@(Ray ror rdr _) t_min t_max =
  case rct of
    (XYRect x0 x1 y0 y1 k rmat) -> return $
      rectHit x0 x1 y0 y1 vecX vecY vecZ (Vec3 (0, 0, 1)) k rmat
    (XZRect x0 x1 z0 z1 k rmat) -> return $
      rectHit x0 x1 z0 z1 vecX vecZ vecY (Vec3 (0, 1, 0)) k rmat
    (YZRect y0 y1 z0 z1 k rmat) -> return $
      rectHit y0 y1 z0 z1 vecY vecZ vecX (Vec3 (1, 0, 0)) k rmat
  where
    rectHit i0 i1 j0 j1 vecI vecJ vecK outwardNormal k mat =
      if (t < t_min) || (t > t_max)
        then Nothing
        else let i = vecI ror + t * vecI rdr
                 j = vecJ ror + t * vecJ rdr
              in if (i < i0) || (i > i1) || (j < j0) || (j > j1)
                   then Nothing
                   else let rec_u = (i - i0) / (i1 - i0)
                            rec_v = (j - j0) / (j1 - j0)
                            p = r `at` t
                            (frontFace, normal) = faceNormal r outwardNormal
                         in Just $
                            Hit t p normal rec_u rec_v frontFace mat
      where
        t = (k - vecK ror) / vecK rdr
hit (Translate offset h) (Ray ror rdr tm) t_min t_max = do
  let m_r = Ray (ror `vecSub` offset) rdr tm
  mh <- hit h m_r t_min t_max
  return $
    case mh of
      Nothing -> Nothing
      Just (Hit t p outwardNormal rec_u rec_v _ mat) ->
        let (mFrontFace, mNormal) = faceNormal m_r outwardNormal
         in Just (Hit t (p `vecAdd` offset) mNormal rec_u rec_v mFrontFace mat)
hit (Rotate axis sin_theta cos_theta _ h) (Ray ror rdr tm) t_min t_max =
  let rotated_r =
        Ray
          (unRotatePoint axis sin_theta cos_theta ror)
          (unRotatePoint axis sin_theta cos_theta rdr)
          tm
   in do
    mh <- hit h rotated_r t_min t_max
    return $
      case mh of
        Nothing -> Nothing
        Just (Hit t p outwardNormal u v _ mat) ->
          let rot_p = rotatePoint axis sin_theta cos_theta p
              rot_outwardNormal =
                rotatePoint axis sin_theta cos_theta outwardNormal
              (rot_frontFace, rot_normal) =
                faceNormal rotated_r rot_outwardNormal
           in Just (Hit t rot_p rot_normal u v rot_frontFace mat)
hit (ConstantMedium nInvD phaseFun boundary) r@(Ray ror rdr tm) t_min t_max =
  undefined
hit sph r@(Ray ror rdr tm) t_min t_max =
  let sc = sphCenter sph tm
      sr = sphRadius sph
      oc = ror `vecSub` sc
      a = dot rdr rdr
      b = seq oc (dot oc rdr)
      c = seq sr (dot oc oc - (sr * sr))
      discriminant = b * b - a * c
   in return $
      if discriminant > 0
        then let sd = sqrt discriminant
                 temp1 = ((-b) - sd) / a
                 temp2 = ((-b) + sd) / a
              in if | t_min < temp1 && temp1 < t_max ->
                      Just $ recHit temp1 r sph
                    | t_min < temp2 && temp2 < t_max ->
                      Just $ recHit temp2 r sph
                    | otherwise -> Nothing
        else Nothing

faceNormal :: Ray -> Vec3 -> (Bool, Vec3)
faceNormal (Ray _ rdr _) outwardNormal =
  let frontFace = rdr `dot` outwardNormal < 0
   in ( frontFace
      , if frontFace
          then outwardNormal
          else vecNegate outwardNormal)

recHit :: Double -> Ray -> Hittable -> Hit
recHit temp r@(Ray _ _ tm) sph =
  let sc = sphCenter sph tm
      sr = sphRadius sph
      sm = sphMaterial sph
      p = r `at` temp
      pShift = p `vecSub` sc
      outwardNormal = divide pShift sr
      (frontFace, normal) = faceNormal r outwardNormal
      (u, v) = let phi = atan2 (vecZ outwardNormal) (vecX outwardNormal)
                   theta = asin (vecY outwardNormal)
        in (1.0 - ((phi + pi) / (2 * pi)), (theta + (pi / 2)) / pi)
   in Hit temp p normal u v frontFace sm

randomDouble :: RandGen -> (Double, RandGen)
randomDouble (RandGen g) =
  let (x, g1) = MT.randomDouble g
   in (x, RandGen g1)

randomDoubleM :: RandomState s Double
randomDoubleM = do
  gRef <- asks getGenRef
  g1 <- lift $ readSTRef gRef
  let (x, g2) = randomDouble g1
  lift $ writeSTRef gRef g2
  return x

randomDoubleRM :: Double -> Double -> RandomState s Double
randomDoubleRM mn mx = do
  rd <- randomDoubleM
  return $ mn + (mx - mn) * rd

randomIntRM :: Int -> Int -> RandomState s Int
randomIntRM lo hi = floor <$> randomDoubleRM (fromIntegral lo) (fromIntegral hi)

randomVec3DoubleM :: RandomState s Vec3
randomVec3DoubleM = do
  gRef <- asks getGenRef
  gen <- lift $ readSTRef gRef
  let (x, g1) = randomDouble gen
  let (y, g2) = randomDouble g1
  let (z, g3) = randomDouble g2
  lift $ writeSTRef gRef g3
  return $ Vec3 (x, y, z)

randomVec3DoubleRM :: Double -> Double -> RandomState s Vec3
randomVec3DoubleRM mn mx = do
  x <- randomDoubleRM mn mx
  y <- randomDoubleRM mn mx
  z <- randomDoubleRM mn mx
  return $ Vec3 (x, y, z)

randomInUnitSphereM :: RandomState s Vec3
randomInUnitSphereM = do
  gRef <- asks getGenRef
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitSphere gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitSphere :: RandGen -> (Vec3, RandGen)
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
  gRef <- asks getGenRef
  gen <- lift $ readSTRef gRef
  let (rUnit, gen1) = randomInUnitDisk gen
  lift $ writeSTRef gRef gen1
  return rUnit

randomInUnitDisk :: RandGen -> (Vec3, RandGen)
randomInUnitDisk gen =
  let (x, g1) = randomDouble gen
      (y, g2) = randomDouble g1
      p = scale 2.0 (Vec3 (x, y, 0)) `vecSub` Vec3 (1.0, 1.0, 0)
   in if squaredLength p < 1.0
        then (p, g2)
        else randomInUnitDisk g2

randomUnitVectorM :: RandomState s Vec3
randomUnitVectorM = do
  gRef <- asks getGenRef
  gen <- lift $ readSTRef gRef
  let (aa, g1) = randomDouble gen
  let a = aa * 2 * pi
  let (zz, g2) = randomDouble g1
  let z = (zz * 2) - 1
  let r = sqrt (1 - z * z)
  lift $ writeSTRef gRef g2
  return $ Vec3 (r * cos a, r * sin a, z)

_randomInHemisphereM :: XYZ -> RandomState s Vec3
_randomInHemisphereM n = do
  inUnitSphere <- randomInUnitSphereM
  if (inUnitSphere `dot` n) > 0.0
    then return inUnitSphere
    else return (vecNegate inUnitSphere)

data Camera = Camera
  XYZ    -- |  camera_origin
  XYZ    -- |  camera_llc
  XYZ    -- |  camera_horiz
  XYZ    -- |  camera_vert
  XYZ    -- |  camera_u
  XYZ    -- |  camera_v
  XYZ    -- |  _camera_w
  Double -- |  camera_lensRadius
  Double -- |  camera_t0
  Double -- |  camera_t1

getRay :: Camera -> Double -> Double -> RandomState s Ray
getRay (Camera c_or c_llc c_horiz c_vert c_u c_v _ c_lr c_time0 c_time1) s t =
  do
    rd <- fmap (scale c_lr) randomInUnitDiskM
    let offset = scale (vecX rd) c_u `vecAdd` scale (vecY rd) c_v
    tm <- randomDoubleRM c_time0 c_time1
    return $ Ray
      (c_or `vecAdd` offset)
      (        c_llc
      `vecAdd` scale s c_horiz
      `vecAdd` scale t c_vert
      `vecSub` c_or
      `vecSub` offset
      )
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
rayColor ray depth = rayColorHelp ray depth id
  where
    rayColorHelp :: Ray -> Int -> (Albedo -> Albedo) -> RayTracingM s Albedo
    rayColorHelp r d alb_acc =
      if d <= 0
        then return $ alb_acc (albedo (0.0, 0.0, 0.0))
        else do
          htbls <- asks getSceneHittables
          mh <- hit htbls r epsilon infinity
          case mh of
            Nothing -> do
              bgd <- asks getBackground
              return $ alb_acc bgd
            Just h@(Hit _ hp _ hu hv _ hm) -> do
              let em@(Albedo emv) = emitted hm hu hv hp
              mscatter <- scatter hm r h
              case mscatter of
                Nothing -> return $ alb_acc em
                Just (sray, Albedo att) ->
                  rayColorHelp
                    sray
                    (d - 1)
                    (\(Albedo new) ->
                       alb_acc $ Albedo $ emv `vecAdd` (att `vecMul` new))

sampleColor :: (Int, Int) -> Albedo -> Int -> RayTracingM s Albedo
sampleColor (x, y) (Albedo accCol) _ = do
  gRef <- asks getGenRef
  gen <- lift $ readSTRef gRef
  maxDepth <- asks getMaxDepth
  let (ru, g1) = randomDouble gen
  let (rv, g2) = randomDouble g1
  imageWidth <- asks getImageWidth
  imageHeight <- asks getImageHeight
  let u = (fromIntegral x + ru) / fromIntegral imageWidth
  let v = (fromIntegral y + rv) / fromIntegral imageHeight
  camera <- asks getCamera
  r <- getRay camera u v
  lift $ writeSTRef gRef g2
  (Albedo c1) <- rayColor r maxDepth
  return $ Albedo $ accCol `vecAdd` c1

renderPos :: (Int, Int) -> RayTracingM s Albedo
renderPos (x, y) = do
  nsPerThread <- asks getNsPerThread
  ns <- asks getNumSamples
  (Albedo summedColor) <-
    foldM
      (sampleColor (x, y))
      (albedo (0.0, 0.0, 0.0))
      [0 .. nsPerThread - 1]
  return $ Albedo $ divide summedColor (fromIntegral ns)

renderRow :: [(Int, Int)] -> RayTracingM s [Albedo]
renderRow = mapM renderPos

pixelPositions :: Int -> Int -> [[(Int, Int)]]
pixelPositions nx ny = map (\y -> map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

runRender :: RenderStaticEnv -> [RandGen] -> [[RGB]]
runRender staticEnv gens =
  let imageWidth = getStaticImageWidth staticEnv
      imageHeight = getStaticImageHeight staticEnv
      pp = pixelPositions imageWidth imageHeight
   in runST $ do
        gensRef <- newSTRef gens
        mapM
          (\rowPs ->
             map albedoToColor <$>
             parallelRenderRow
               rowPs
               staticEnv
               gensRef)
          pp

parallelRenderRow ::
     [(Int, Int)]
  -> RenderStaticEnv
  -> STRef s [RandGen]
  -> ST s [Albedo]
parallelRenderRow rowps staticEnv gsRef = do
  gs <- readSTRef gsRef
  let (sampleGroups, newGs) = unzip $ parMap
        rpar
        (\gen -> force $ runST $ do
          genRef <- newSTRef gen
          runReaderT
            (do
              row <- renderRow rowps
              g   <- lift $ readSTRef genRef
              return (row, g)
            )
            (mkRenderEnv staticEnv genRef)
        )
        gs
  writeSTRef gsRef newGs
  return $ foldl'
    (zipWith (\(Albedo a1) (Albedo a2) -> Albedo $ vecAdd a1 a2))
    (replicate (getStaticImageWidth staticEnv) (albedo (0.0, 0.0, 0.0)))
    sampleGroups


-- Scenes

makeCornellBoxScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeCornellBoxScene t0 t1 gen = runST $ do
  gRef  <- newSTRef gen
  world <- runReaderT
    (do
      let red   = Lambertian $ ConstantColor (albedo (0.65, 0.05, 0.05))
      let white = Lambertian $ ConstantColor (albedo (0.73, 0.73, 0.73))
      let green = Lambertian $ ConstantColor (albedo (0.12, 0.45, 0.15))
      let light = DiffuseLight $ ConstantColor (albedo (15, 15, 15))
      makeBVH
        (Just (t0, t1))
        (   rect YZPlane 0   555 0   555 555 green
        :<| rect YZPlane 0   555 0   555 0   red
        :<| rect XZPlane 213 343 227 332 554 light
        :<| rect XZPlane 0   555 0   555 0   white
        :<| rect XZPlane 0   555 0   555 555 white
        :<| rect XYPlane 0   555 0   555 555 white
        :<| translate (Vec3 (265, 0, 295)) (rotate YAxis 15
              (cuboid (Vec3 (0, 0, 0)) (Vec3 (165, 330, 165)) white))
        :<| translate (Vec3 (130, 0, 65))
             (rotate YAxis (-18) (rotate ZAxis 30
              (cuboid (Vec3 (0, 0, 0)) (Vec3 (165, 165, 165)) white)))
        :<| Empty
        )
    )
    (dummyRenderEnv gRef)
  g1 <- readSTRef gRef
  return ((world, albedo (0.0, 0.0, 0.0)), g1)

cornellCamera :: (Int, Int) -> Camera
cornellCamera (imageWidth, imageHeight) =
  newCamera
    (Vec3 (278, 278, -800))
    (Vec3 (278, 278, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    40.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    10.0
    0.0
    1.0

makeSimpleLightScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeSimpleLightScene t0 t1 gen = runST $ do
  gRef  <- newSTRef gen
  world <- runReaderT
    (do
      perText <- makePerlin 1.0
      let difflight = DiffuseLight $ ConstantColor $ albedo (4, 4, 4)
      makeBVH
        (Just (t0, t1))
        (   sphere (Vec3 (0, -1000, 0)) 1000 (Lambertian perText)
--               :<| Sphere (Vec3 (0, 2, 0)) 2 (Lambertian (ConstantColor $ albedo (0.5, 0.0, 0.3)))
        :<| sphere (Vec3 (0, 2, 0))     2    (Lambertian perText)
        :<| sphere (Vec3 (0, 7, 0))     2    difflight
        :<| rect XYPlane 3 5 1 3 (-2) difflight
        :<| Empty
        )
    )
    (dummyRenderEnv gRef)
  g1 <- readSTRef gRef
  return ((world, albedo (0.0, 0.0, 0.0)), g1)

earthTexture :: IO Texture
earthTexture = do
  earthImg <- JP.readImage "./earthmap.jpg"
  let (earthIm, w, h) = case earthImg of
        Left  _ -> (Nothing, 0, 0)
        Right x -> let im = JP.convertRGB8 x
                   in (Just (Image im), JP.imageWidth im, JP.imageHeight im)
  return $ ImageTexture earthIm w h

makeEarthScene :: Texture -> Time -> Time -> RandGen -> (Scene, RandGen)
makeEarthScene earthTex t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    world <-
      runReaderT
        (makeBVH
           (Just (t0, t1))
           (Sphere (Vec3 (0, 0, 0)) 2 (Lambertian earthTex) :<| Empty))
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0, 0, 0)), g1)

twoSpheresSceneCamera :: (Int, Int) -> Camera
twoSpheresSceneCamera (imageWidth, imageHeight) =
  newCamera
    (Vec3 (26.0, 4.0, 6.0))
    (Vec3 (0.0, 2.0, 0.0))
    (Vec3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    20.0
    0.0
    1.0

makeTwoPerlinSpheresScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeTwoPerlinSpheresScene t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    world <-
      runReaderT
        (do perText <- makePerlin 1.5
            makeBVH
              (Just (t0, t1))
              (Sphere (Vec3 (0, -1000, 0)) 1000 (Lambertian perText) :<|
               Sphere (Vec3 (0, 2, 0)) 2 (Lambertian perText) :<|
               Empty))
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0, 0, 0)), g1)

makeTwoSpheresScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeTwoSpheresScene t0 t1 gen =
  runST $ do
    let checkerMaterial =
          Metal
            (CheckerTexture
               (ConstantColor $ albedo (0.2, 0.3, 0.1))
               (ConstantColor $ albedo (0.9, 0.9, 0.9)))
            (Fuzz 0.0)
    let flatMaterial =
          Lambertian (ConstantColor $ albedo (0.6, 0.2, 0.1))
    gRef <- newSTRef gen
    world <-
      runReaderT
        (makeBVH
           (Just (t0, t1))
           (Sphere (Vec3 (0, -10, 0)) 10 checkerMaterial :<|
            Sphere (Vec3 (0, 10, 0)) 10 flatMaterial :<|
            Empty))
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0, 0, 0)), g1)

randomSceneCamera :: (Int, Int) -> Camera
randomSceneCamera (imageWidth, imageHeight) =
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
makeRandomScene :: Texture -> Time -> Time -> RandGen -> (Scene, RandGen)
makeRandomScene earthtex _ _ gen =
  runST $ do
    gRef <- newSTRef gen
    world <- runReaderT makeSceneM (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return (world, g1)
  where
    makeSceneM :: RandomState s Scene
    makeSceneM = do
      let ns = [(x, y) | x <- [-11 .. 10], y <- [-11 .. 10]]
      let ground =
            sphere
              (Vec3 (0.0, -1000.0, 0.0))
              1000
              -- (Lambertian (ConstantColor $ albedo (0.5, 0.5, 0.5))) --gray
              (Lambertian
                 (CheckerTexture
                    (ConstantColor $ albedo (0.2, 0.3, 0.1))
                    (ConstantColor $ albedo (0.9, 0.9, 0.9))))
      let s1
            -- Sphere (Vec3 (0.0, 1.0, 0.0)) 1.0 (Dielectric (RefractiveIdx 1.5))
           =
            cuboid
              (Vec3 (-0.75, 0.0, -0.75))
              (Vec3 (0.75, 1.5, 0.75))
              (Dielectric (RefractiveIdx 1.5))
      let s2 =
            sphere
              (Vec3 (-4.0, 1.0, 0.0))
              1.0
              -- (Lambertian (ConstantColor $ albedo (0.4, 0.2, 0.1)))
              (Lambertian earthtex)
      let s3 =
            sphere
              (Vec3 (4.0, 1.0, 0.0))
              1.0
              (Metal (ConstantColor $ albedo (0.7, 0.6, 0.5)) (Fuzz 0.0))
      nps <- catMaybes <$> mapM makeRandomSphereM ns
      world <-
        makeBVH (Just (0.0, 1.0)) $
        ground :<| s1 :<| s2 :<| s3 :<| S.fromList nps
      return (world, albedo (0.7, 0.8, 0.9))
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
                     let alb = Albedo $ a1 `vecMul` a2
                     return $
                       Just $
                       movingSphere
                         center
                         (center `vecAdd` Vec3 (sph_move_x, 0, sph_move_z))
                         0.0
                         1.0
                         0.2
                         (Lambertian (ConstantColor alb))
                | mat < 0.95 -- Metal
                 ->
                  do alb <- Albedo <$> randomVec3DoubleRM 0.5 1.0
                     fuzz <- randomDoubleRM 0.0 0.5
                     return $
                       Just $
                       sphere center 0.2 (Metal (ConstantColor alb) (Fuzz fuzz))
                | otherwise --Glass
                 ->
                  return $
                  Just $ sphere center 0.2 (Dielectric (RefractiveIdx 1.5))
