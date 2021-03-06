{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( (|+|)
    , (|-|)
    , (|*|)
    , albedo
    , boundingBox
    , constantMedium
    , cuboid
    , dummyRenderEnv
    , Lib.length
    , makeBVH
    , makePerlin
    , mkRenderStaticEnv
    , movingSphere
    , newCamera
    , newRandGen
    , point3
    , printRow
    , randomDoubleM
    , randomDoubleRM
    , randomVec3DoubleM
    , randomVec3DoubleRM
    , randGen
    , rect
    , rotate
    , runRender
    , sphere
    , surroundingBox
    , translate
    , vecX
    , vecY
    , vecZ
    , Albedo(..)
    , Axis(..)
    , Box(..)
    , Camera
    , Fuzz(..)
    , Hittable(..)
    , Image(..)
    , Material(..)
    , Plane(..)
    , RandGen
    , RandomState
    , RefractiveIdx(..)
    , Scene
    , Texture(..)
    , Time
    , Vec3) where

import qualified Codec.Picture               as JP (Image (..), PixelRGB8 (..),
                                                    pixelAt)
import           Control.DeepSeq             (NFData, force, rnf)
import           Control.Monad.Reader
import           Control.Monad.ST.Lazy       (ST, runST, strictToLazyST)
import           Control.Parallel.Strategies (parTraversable, rpar, using)
import           Data.Bits                   (xor)
import           Data.Foldable               (toList)
import qualified Data.Map.Strict             as M
import           Data.Sequence               (Seq (..))
import qualified Data.Sequence               as S
import           Data.STRef.Lazy
import qualified Data.Vector                 as VV (Vector, fromList, map,
                                                    toList, unzip, zip, (!))
import           Data.Vector.Unboxed         (Vector, enumFromN, freeze, thaw)
import qualified Data.Vector.Unboxed         as V ((!))
import qualified Data.Vector.Unboxed.Mutable as MV (swap)
import           Data.Word                   (Word8)
import           Random
import           System.IO                   (Handle, hPutStr, hPutStrLn,
                                              stderr)

-- Epsilon (some smallish number)
epsilon :: Double
epsilon = 0.0001

infinity :: Double
infinity = read "Infinity" :: Double

-- A scene should be a BVHNode of objects, a BVHNode of lights
-- (BVHNodes are a Hittable), and background color
type Scene = (Hittable, Hittable, Albedo)

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
getStaticImageHeight (RenderStaticEnv (_, _, (_, height), _, _, _, _)) = height

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
    (point3 (13.0, 2.0, 3.0))
    (point3 (0.0, 0.0, 0.0))
    (point3 (0.0, 1.0, 0.0))
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
            (point3 (0, 0, 0))
            1.0
            (Lambertian $ ConstantColor $ albedo (0, 1.0, 1.0))
        , Unhittable
        , albedo (0, 0, 0))
   in mkRenderStaticEnv world dummyCamera (0, 0) 0 0 0

dummyRenderEnv :: STRef s RandGen -> RenderEnv s
dummyRenderEnv = mkRenderEnv dummyRenderStaticEnv

getSceneHittables :: RenderEnv s -> Hittable
getSceneHittables
  (RenderEnv (RenderStaticEnv ((scn, _, _), _, _, _, _, _, _), _)) = scn

getSceneLights :: RenderEnv s -> Hittable
getSceneLights
  (RenderEnv (RenderStaticEnv ((_, lights, _), _, _, _, _, _, _), _)) = lights

getBackground :: RenderEnv s -> Albedo
getBackground
  (RenderEnv (RenderStaticEnv ((_, _, bkgd), _, _, _, _, _, _), _)) = bkgd

getCamera :: RenderEnv s -> Camera
getCamera (RenderEnv (RenderStaticEnv (_, cam, _, _, _, _, _), _)) = cam

_getImageWidth :: RenderEnv s -> Int
_getImageWidth (RenderEnv (staticEnv, _)) = getStaticImageWidth staticEnv

_getImageHeight :: RenderEnv s -> Int
_getImageHeight (RenderEnv (staticEnv, _)) = getStaticImageHeight staticEnv

getNumSamples :: RenderEnv s -> Int
getNumSamples (RenderEnv (RenderStaticEnv (_, _, _, numSamples, _, _, _), _)) =
  numSamples

getMaxDepth :: RenderEnv s -> Int
getMaxDepth (RenderEnv (RenderStaticEnv (_, _, _, _, maxDepth, _, _), _)) =
  maxDepth

_getNsPerThread :: RenderEnv s -> Int
_getNsPerThread (RenderEnv (RenderStaticEnv (_, _, _, _, _, _, nsPerThread), _))
  = nsPerThread

getGenRef :: RenderEnv s -> STRef s RandGen
getGenRef (RenderEnv (_, genRef)) = genRef

type RayTracingM s = RandomState s

-- Final representation of a color of a pixel before output
newtype RGB = RGB (Word8, Word8, Word8)

instance Show RGB where
  show (RGB (r, g, b)) = unwords [show r, show g, show b]

instance NFData RGB where
  rnf (RGB (r, g, b)) = rnf r `seq` rnf g `seq` rnf b

-- General 3-dimensional Doubles--could be color or vector or position
type Point3 = Vec3

point3 :: (Double, Double, Double) -> Point3
point3 (x, y, z) = Vec3 x y z

data Vec3 = Vec3 !Double !Double !Double

instance NFData Vec3 where
  rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z

vecX :: Vec3 -> Double
vecX (Vec3 x _ _) = x

vecY :: Vec3 -> Double
vecY (Vec3 _ y _) = y

vecZ :: Vec3 -> Double
vecZ (Vec3 _ _ z) =  z

instance Show Vec3 where
  show (Vec3 x y z) = unwords [show x, show y, show z]

infixl 7 |*|
(|*|) :: Vec3 -> Vec3 -> Vec3
(|*|) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

_vecDiv :: Vec3 -> Vec3 -> Vec3
_vecDiv (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 / x2) (y1 / y2) (z1 / z2)

infixl 7 |+|
(|+|) :: Vec3 -> Vec3 -> Vec3
(|+|) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

infixl 7 |-|
(|-|) :: Vec3 -> Vec3 -> Vec3
(|-|) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

vecNegate :: Vec3 -> Vec3
vecNegate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)

length :: Vec3 -> Double
length vec = sqrt (squaredLength vec)

squaredLength :: Vec3 -> Double
squaredLength (Vec3 x y z) = (x * x) + (y * y) + (z * z)

makeUnitVector :: Vec3 -> Vec3
makeUnitVector v = divide v (Lib.length v)

scale :: Double -> Vec3 -> Vec3
scale k (Vec3 x y z) = Vec3 (x * k) (y * k) (z * k)

divide :: Vec3 -> Double -> Vec3
divide (Vec3 x y z) k = Vec3 (x / k) (y / k) (z / k)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  point3 (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

data ONB = ONB { onbU :: !Vec3, onbV :: !Vec3, onbW :: !Vec3 }
  deriving Show

onbLocal :: ONB -> Double -> Double -> Double -> Vec3
onbLocal (ONB u v w) a b c = scale a u |+| scale b v |+| scale c w

onbLocalV :: ONB -> Vec3  -> Vec3
onbLocalV (ONB u v w) (Vec3 a b c) = scale a u |+| scale b v |+| scale c w

onbFromW :: Vec3 -> ONB
onbFromW n = let w = makeUnitVector n
                 a = if abs (vecX w) > 0.9
                     then Vec3 0.0 1.0 0.0
                     else Vec3 1.0 0.0 0.0
                 v = makeUnitVector (cross w a)
                 u = cross w v
             in ONB u v w

clamp :: (Double, Double) -> Double -> Double
clamp (mn, mx) x =
  if | x < mn -> mn
     | x > mx -> mx
     | otherwise -> x

scaleColor :: Double -> Word8
scaleColor x = floor $ 256 * clamp (0.0, 0.999) (sqrt x)

albedoToColor :: Albedo -> RGB
albedoToColor (Albedo (Vec3 x y z)) =
  RGB (scaleColor x, scaleColor y, scaleColor z)

colorToAlbedo :: RGB -> Albedo
colorToAlbedo (RGB (r, g, b)) =
  albedo
    (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0)

printRow :: Handle -> Int -> (Int, VV.Vector RGB) -> IO ()
printRow handle imageHeight (i, row) = do
  hPutStr stderr ("\rRendering row " ++ show i ++ " of " ++ show imageHeight)
  hPutStrLn handle $ showRow (VV.toList row)

showRow :: [RGB] -> String
showRow row = unwords $ fmap show row

data Ray
  = Ray
      !Point3
      -- ^ ray_origin
      !Point3
      -- ^ ray_direction
      !Double
      -- ^ ray_time
  deriving (Show)

at :: Ray -> Double -> Point3
at (Ray orn dr _) t = orn |+| scale t dr

data Hit
  = Hit
      !Double
      -- ^ hit_t
      !Point3
      -- ^ hit_p
      !Point3
      -- ^ hit_normal - vector normal to the surface of the object
      -- at the point of the hit
      !Double
      -- ^ hit_u
      !Double
      -- ^ hit_v
      !Bool
      -- ^ hit_frontFace --  did the ray hit the outer face of the
      -- object?                   -
      !Material
      -- ^ hit_material}

data Material
  = Lambertian !Texture
  | Metal !Texture !Fuzz
  | Dielectric !RefractiveIdx
  | DiffuseLight !Texture
  | Isotropic !Texture
  deriving Show

newtype Fuzz = Fuzz Double
  deriving Show

newtype RefractiveIdx = RefractiveIdx Double
  deriving Show

newtype Albedo = Albedo Vec3
  deriving Show

albedo :: (Double, Double, Double) -> Albedo
albedo (r, g, b) = Albedo $ Vec3 r g b

instance NFData Albedo where
  rnf (Albedo v) = rnf v `seq` ()

data Pdf = CosinePdf ONB
         | HittablePdf Point3 Hittable
         | MixturePdf {mixPdf1 :: Pdf, mixPdf2 :: Pdf}
  deriving Show

pdfGenerate :: Pdf -> RandomState s Vec3
pdfGenerate (CosinePdf uvw) = fmap (onbLocalV uvw) randomCosineDirection
pdfGenerate (HittablePdf o htbl) = htblRandom htbl o
pdfGenerate (MixturePdf pdf1 pdf2) = do
  rd <- randomDoubleM
  if rd < 0.5 then pdfGenerate pdf1 else pdfGenerate pdf2

pdfValue :: Pdf -> Vec3 -> RandomState s Double
pdfValue (CosinePdf uvw) direction =
  let cosine = dot (makeUnitVector direction) (onbW uvw)
  in return $ if cosine <= 0 then 0 else cosine / pi
pdfValue (HittablePdf o htbl) direction = htblPdfValue htbl o direction
pdfValue (MixturePdf pdf1 pdf2) direction = do
  v1 <- pdfValue pdf1 direction
  v2 <- pdfValue pdf2 direction
  return $ 0.5 * (v1 + v2)

newtype Image = Image (JP.Image JP.PixelRGB8)

pixelAt :: Image -> Int -> Int -> RGB
pixelAt (Image im) i j =
  let (JP.PixelRGB8 r g b) = JP.pixelAt im i j
   in RGB (r, g, b)

instance Show Image where
  show _ = "<Image>"

data Texture
  = ConstantColor Albedo
  | CheckerTexture
      Texture
      -- ^ checkerTextureOdd
      Texture
      -- ^ checkerTextureEven
  | Perlin
      (VV.Vector Vec3)
      -- ^ perlinRanFloat
      (Vector Int)
      -- ^ perlinPermX
      (Vector Int)
      -- ^ perlinPermY
      (Vector Int)
      -- ^ perlinPermZ
      Double
      -- ^ perlinScale
  | ImageTexture
      (Maybe Image)
      -- ^ imageTexture_image
      Int
      -- ^ imageTexture_width
      Int
      -- ^ imageTexture_height
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
  let (Vec3 pX' pY' pZ') = scale sc p
      i = floor pX'
      j = floor pY'
      k = floor pZ'
      u = pX' - fromIntegral i
      v = pY' - fromIntegral j
      w = pZ' - fromIntegral k
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
noise (ConstantColor _) _ = error "ConstantColor does not support noise"
noise (CheckerTexture _ _) _ = error "CheckerTexture does not support noise"
noise ImageTexture {} _ = error "ImageTexture does not support noise"

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
            (val `dot` point3 (u - i, v - j, w - k))))
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

textureValue :: Texture -> Double -> Double -> Point3 -> Albedo
textureValue (ConstantColor color) _ _ _ = color
textureValue (CheckerTexture oddTex evenTex) u v p =
  if sin (10 * vecX p) * sin (10 * vecY p) * sin (10 * vecZ p) < 0
  then textureValue oddTex u v p
  else textureValue evenTex u v p
textureValue ptex@Perlin{} _ _ p =
  Albedo $ scale (marbleTexture ptex p) $ point3 (1.0, 1.0, 1.0)
textureValue (ImageTexture (Just im) nx ny) u v _ =
  let nxd = fromIntegral nx
      i = floor $ clamp (0, nxd - epsilon) (u * nxd)
      nyd = fromIntegral ny
      j = floor $ clamp (0, nyd - epsilon) ((1.0 - v) * nyd - epsilon)
   in colorToAlbedo $ pixelAt im i j
textureValue (ImageTexture Nothing _ _) _ _ _ = albedo (0, 1, 1)

marbleTexture :: Texture -> Point3 -> Double
marbleTexture ptex p = 0.5 * (1.0 + sin (vecZ p + 10 * turb ptex p 7))

data Axis = XAxis | YAxis | ZAxis
  deriving Show

data Plane = XYPlane | XZPlane | YZPlane
  deriving Show

data Hittable
  = Sphere
      !Point3
      -- ^ sphere_center
      !Double
      -- ^ sphere_radius
      !Material
      -- ^ sphere_material
  | MovingSphere
      !Point3
      -- ^ msphere_center0
      !Point3
      -- ^ msphere_center1
      !Time
      -- ^ msphere_time0
      !Time
      -- ^ msphere_time1
      !Time
      -- ^ duration
      !Double
      -- ^ msphere_radius
      !Material
      -- ^ msphere_material
  | Rect !Rectangle
  | Cuboid
      !Point3
      -- ^ box min
      !Point3
      -- ^ box max
      ![Rectangle]
      -- ^ Rects of the six sides of the box
  | BVHNode
      Hittable
      -- ^ bvh_left
      Hittable
      -- ^ bvh_right
      !Box
      -- ^ bvh_box
      !Int
      -- ^^ bvh_size
  | Translate
      !Point3
      -- ^ translation offset
      !Hittable
      -- ^ the translated Hittable
  | Rotate
      !Axis
      -- ^ the axis of rotation
      !Double
      -- ^ sin theta
      !Double
      -- ^ cos theta
      !Box
      -- ^ the BoundingBox
      !Hittable
      -- ^ the rotated Hittable
  | ConstantMedium
      !Double
      -- ^ negative inverse density of the constant medium
      !Material
      -- ^ material of the constant medium
      !Hittable
      -- ^ the shape of the constant medium
  | Unhittable
  deriving (Show)

sphere :: Point3 -> Double -> Material -> Hittable
sphere = Sphere

movingSphere ::
     Point3 -> Point3 -> Time -> Time -> Double -> Material -> Hittable
movingSphere c0 c1 t0 t1 = MovingSphere c0 c1 t0 t1 (t1 - t0)

cuboid :: Point3 -> Point3 -> Material -> Hittable
cuboid bmin@(Vec3 x0 y0 z0) bmax@(Vec3 x1 y1 z1) mat =
  Cuboid
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
  = XYRect
      !Double
      -- ^ xyrect_x0
      !Double
      -- ^ xyrect_x1
      !Double
      -- ^ xyrect_y0
      !Double
      -- ^ xyrect_y1
      !Double
      -- ^ xyrect_k
      !Material
      -- ^ xyrect_material
  | XZRect
      !Double
      -- ^ _xzrect_x0
      !Double
      -- ^ _xzrect_x1
      !Double
      -- ^ _xzrect_z0
      !Double
      -- ^ _xzrect_z1
      !Double
      -- ^ _xzrect_k
      !Material
      -- ^ _xzrect_material
  | YZRect
      !Double
      -- ^ _yzrect_y0
      !Double
      -- ^ _yzrect_y1
      !Double
      -- ^ _yzrect_z0
      !Double
      -- ^ _yzrect_z1
      !Double
      -- ^ _yzrect_k
      !Material
      -- ^ _yzrect_material
  deriving (Show)

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

htblSize :: Hittable -> Int
htblSize Unhittable = 0
htblSize (Sphere {}) = 1
htblSize (MovingSphere {}) = 1
htblSize (Rect {}) = 1
htblSize (Cuboid {}) = 1
htblSize (ConstantMedium _ _ _) = 1
htblSize (Translate _ htbl) = htblSize htbl
htblSize (Rotate _ _ _ _ htbl) = htblSize htbl
htblSize (BVHNode _ _ _ size) = size

htblPdfValue :: Hittable -> Point3 -> Vec3 -> RandomState s Double
htblPdfValue htbl origin v = do
  genRef <- asks getGenRef
  g1 <- lift $ readSTRef genRef
  case hit htbl (Ray origin v 0.0) epsilon infinity g1 of
    (Nothing, g2) -> do
      lift $ writeSTRef genRef g2
      return 0.0
    (Just (Hit ht _ hn _ _ _ _), g2) -> do
      lift $ writeSTRef genRef g2
      case htbl of
        (Rect (XZRect x0 x1 z0 z1 _ _))
          -> let area = (x1 - x0) * (z1 - z0)
                 distanceSquared = ht * ht * squaredLength v
                 cosine = abs $ dot v hn / Lib.length v
             in return $ distanceSquared / (cosine * area)
        (Sphere center radius _)
          -> let cosThetaMax = sqrt
                   (1 - radius * radius / squaredLength (center |-| origin))
                 solidAngle = 2 * pi * (1 - cosThetaMax)
             in return $ 1 / solidAngle
        (BVHNode bvh_left bvh_right _ size) -> do
          leftPdf <- foldPdf 0 bvh_left
          let leftWeight = fromIntegral (htblSize bvh_left)
                / fromIntegral size
          rightPdf <- foldPdf 0 bvh_right
          let rightWeight = fromIntegral (htblSize bvh_right)
                / fromIntegral size
          return $ leftWeight * leftPdf + rightWeight * rightPdf
        _ -> return 0.0
  where
    foldPdf :: Double -> Hittable -> RandomState s Double
    foldPdf acc h = fmap (+ acc) $ htblPdfValue h origin v

htblRandom :: Hittable -> Vec3 -> RandomState s Vec3
htblRandom (Rect (XZRect x0 x1 z0 z1 k _) ) o = do
  rx <- randomDoubleRM x0 x1
  rz <- randomDoubleRM z0 z1
  let rp = point3 (rx, k, rz)
  return $ rp |-| o
htblRandom (Sphere center rad _) o = do
  let dir = center |-| o
  let distSquared = squaredLength dir
  let uvw = onbFromW dir
  rts <- randomToSphereM rad distSquared
  return $ onbLocalV uvw rts
htblRandom (BVHNode bvh_left bvh_right _ size) o = do
  rd <- randomDoubleM
  if rd < (fromIntegral $ htblSize bvh_left) / (fromIntegral size)
    then htblRandom bvh_left o
    else htblRandom bvh_right o
htblRandom _ _ = return $ Vec3 1 0 0

translate :: Point3 -> Hittable -> Hittable
translate = Translate

degreesToRadians :: Double -> Double
degreesToRadians angle = angle * pi / 180.0

rotate :: Axis -> Double -> Hittable -> Hittable
rotate axis angle h = Rotate axis sin_theta cos_theta (Box h_min h_max) h
  where
    rad = degreesToRadians angle
    sin_theta = sin rad
    cos_theta = cos rad
    (Box (Vec3 bbMinX bbMinY bbMinZ) (Vec3 bbMaxX bbMaxY bbMaxZ)) =
      boundingBox h Nothing
    updateMinMax ::
         (Double, Double, Double) -> (Point3, Point3) -> (Point3, Point3)
    updateMinMax (i, j, k) (Vec3 minX minY minZ, Vec3 maxX maxY maxZ) =
      let x = i * bbMaxX + (1 - i) * bbMinX
          y = j * bbMaxY + (1 - j) * bbMinY
          z = k * bbMaxZ + (1 - k) * bbMinZ
          Vec3 newX newY newZ =
            rotatePoint axis sin_theta cos_theta (point3 (x, y, z))
       in ( point3
              ( min newX minX
              , min newY minY
              , min newZ minZ)
          , point3
              ( max newX maxX
              , max newY maxY
              , max newZ maxZ))
    (h_min, h_max) =
      foldr
        updateMinMax
        ( point3 (infinity, infinity, infinity)
        , point3 (-infinity, -infinity, -infinity))
        [(i, j, k) | i <- [0, 1, 2], j <- [0, 1, 2], k <- [0, 1, 2]]

rotatePoint :: Axis -> Double -> Double -> Point3 -> Point3
rotatePoint axis sin_theta cos_theta (Vec3 pX pY pZ) =
  case axis of
    XAxis ->
      point3
        (pX, cos_theta * pY - sin_theta * pZ, sin_theta * pY + cos_theta * pZ)
    YAxis ->
      point3
        (cos_theta * pX + sin_theta * pZ, pY, -sin_theta * pX + cos_theta * pZ)
    ZAxis ->
      point3
        (cos_theta * pX - sin_theta * pY, sin_theta * pX + cos_theta * pY, pZ)

unRotatePoint :: Axis -> Double -> Double -> Point3 -> Point3
unRotatePoint axis sin_theta cos_theta (Vec3 pX pY pZ) =
  case axis of
    XAxis ->
      point3
        (pX, cos_theta * pY + sin_theta * pZ, -sin_theta * pY + cos_theta * pZ)
    YAxis ->
      point3
        (cos_theta * pX - sin_theta * pZ, pY, sin_theta * pX + cos_theta * pZ)
    ZAxis ->
      point3
        (cos_theta * pX + sin_theta * pY, -sin_theta * pX + cos_theta * pY, pZ)

constantMedium :: Double -> Texture -> Hittable -> Hittable
constantMedium density tex =
  ConstantMedium (-1 / density) (Isotropic tex)

data Box = Box
  { box_min :: !Point3
  , box_max :: !Point3
  } deriving (Show)

boxRayIntersect :: Box -> Ray -> Double -> Double -> Bool
boxRayIntersect (Box (Vec3 minX minY minZ) (Vec3 maxX maxY maxZ)) (Ray (Vec3 orX orY orZ) (Vec3 drX drY drZ) _) t_min t_max =
  all
    (\(ror, rdr, mn, mx) ->
       let ta = (mn - ror) / rdr
           tb = (mx - ror) / rdr
           (t0, t1) =
             if ta < tb
               then (ta, tb)
               else (tb, ta)
           tmin = max t0 t_min
           tmax = min t1 t_max
        in tmax > tmin)
    [ (orX, drX, minX, maxX)
    , (orY, drY, minY, maxY)
    , (orZ, drZ, minZ, maxZ)
    ]

type Time = Double


data ScatterRecord = ScatterRecord Ray Bool Albedo Double
  deriving Show

scatter :: Material -> Ray -> Hit -> RandomState s (Maybe ScatterRecord)
scatter (Lambertian tex) (Ray _ _ rtime) (Hit _ hp hn hu hv _ _) = do
  -- rUnit <- randomUnitVectorM
  -- let direction = onbLocalV uvw rCosDir
  -- let scatterDirection = makeUnitVector direction
  -- let scattered = Ray hp scatterDirection rtime
  let att = textureValue tex hu hv hp
  lights <- asks getSceneLights
  let lightsPdf = HittablePdf hp lights
  let cosinePdf = CosinePdf $ onbFromW hn
  let mixPdf = MixturePdf lightsPdf cosinePdf
  pdfD <- pdfGenerate mixPdf
  let scattered = Ray hp (makeUnitVector pdfD) rtime
  pdfVal <- pdfValue mixPdf (makeUnitVector pdfD)
  return $ Just $ ScatterRecord scattered False att pdfVal
scatter (Metal tex (Fuzz fuzz)) (Ray _ rdr rtime) (Hit _ hp hn hu hv _ _) = do
  rUnit <- randomUnitVectorM
  let reflected = reflect (makeUnitVector rdr) hn
  let scattered = Ray hp (reflected |+| scale fuzz rUnit) rtime
  return $ Just $ ScatterRecord scattered True (textureValue tex hu hv hp) 0.0
scatter
  (Dielectric (RefractiveIdx ref_idx))
  (Ray _ rdr rtime)
  (Hit _ hp hn _ _ hff _) = do
  let alb = albedo (1.0, 1.0, 1.0)
  let etaiOverEtat = if hff
                     then 1.0 / ref_idx
                     else ref_idx
  let unitDirection = makeUnitVector rdr
  let cosTheta = min (dot (vecNegate unitDirection) hn) 1.0
  let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
  rd <- randomDoubleM
  return
    $ if (etaiOverEtat * sinTheta > 1.0) || rd < schlick cosTheta etaiOverEtat
      then let reflected = reflect unitDirection hn
           in Just (ScatterRecord (Ray hp reflected rtime) True alb 1.0)
      else let refracted = refract unitDirection hn etaiOverEtat
           in Just (ScatterRecord (Ray hp refracted rtime) True alb 1.0)
scatter DiffuseLight {} _ _  = return Nothing
scatter (Isotropic tex) (Ray _ _ rtime) (Hit _ hp _ hu hv _ _) = do
  randDr <- randomInUnitSphereM
  let scattered = Ray hp randDr rtime
  let attenuation = textureValue tex hu hv hp
  return $ Just (ScatterRecord scattered False attenuation 1.0)

scatteringPdf :: Material -> Ray -> Hit -> Ray -> Double
scatteringPdf (Metal _ _) _ _ _ =
  error "Metal does not support scatteringPdf"
scatteringPdf (Dielectric _) _ _ _ =
  error "Dielectric does not support scatteringPdf"
scatteringPdf (DiffuseLight _) _ _ _ =
  error "DiffuseLight does not support scatteringPdf"
scatteringPdf _  _ (Hit _ _ recNormal _ _ _ _) (Ray _ scatteredDirection _) =
  let cosine = dot recNormal scatteredDirection
  in if cosine < 0
     then 0
     else cosine / pi

emitted :: Material -> Ray -> Hit -> Double -> Double -> Point3 -> Albedo
emitted (DiffuseLight tex) _ (Hit _ _ _ _ _ ff _) u v p =
  if not ff
  then textureValue tex u v p
  else albedo (0, 0, 0)
emitted _ _ _ _ _ _                  = albedo (0, 0, 0)

reflect :: Point3 -> Point3 -> Point3
reflect v n = v |-| scale (2.0 * dot v n) n

refract :: Point3 -> Point3 -> Double -> Point3
refract v n etaiOverEtat =
  let uv = makeUnitVector v
      cosTheta = dot (vecNegate uv) n
      rOutParallel = scale etaiOverEtat (uv |+| scale cosTheta n)
      rOutPerp = scale (-sqrt (1.0 - squaredLength rOutParallel)) n
   in rOutParallel |+| rOutPerp

-- Christopher Schlick approximation for reflectivity of glass based on angle
schlick :: Double -> Double -> Double
schlick cosine ref_idx =
  let r0 = (1.0 - ref_idx) / (1.0 + ref_idx)
      r1 = r0 * r0
   in r1 + (1.0 - r1) * (1 - cosine) ** 5

boundingBox :: Hittable -> Maybe (Time, Time) -> Box
boundingBox (Sphere c r _) _ =
  let rad = point3 (r, r, r)
   in Box (c |-| rad) (c |+| rad)
boundingBox (MovingSphere c0 c1 _ _ _ r _) _ =
  let rad = point3 (r, r, r)
      box0 = Box (c0 |-| rad) (c0 |+| rad)
      box1 = Box (c1 |-| rad) (c1 |+| rad)
   in surroundingBox box0 box1
boundingBox (Rect (XYRect x0 x1 y0 y1 k _)) _ =
  Box (point3 (x0, y0, k - epsilon)) (point3 (x1, y1, k + epsilon))
boundingBox (Rect (XZRect x0 x1 z0 z1 k _)) _ =
  Box (point3 (x0, k - epsilon, z0)) (point3 (x1, k + epsilon, z1))
boundingBox (Rect (YZRect y0 y1 z0 z1 k _)) _ =
  Box (point3 (k - epsilon, y0, z0)) (point3 (k + epsilon, y1, z1))
boundingBox (BVHNode _ _ box _) _ = box
boundingBox (Cuboid c_min c_max _) _ = Box c_min c_max
boundingBox (Translate offset h) mtime =
  let (Box b_min b_max) = boundingBox h mtime
   in Box (b_min |+| offset) (b_max |+| offset)
boundingBox (Rotate _ _ _ box _) _ = box
boundingBox (ConstantMedium _ _ h) mt = boundingBox h mt
boundingBox (Unhittable) _ = error "Should not be trying to bound an Unhittable"

surroundingBox :: Box -> Box -> Box
surroundingBox (Box b0min b0max) (Box b1min b1max) =
  let Vec3 b0min_x b0min_y b0min_z = b0min
      Vec3 b0max_x b0max_y b0max_z = b0max
      Vec3 b1min_x b1min_y b1min_z = b1min
      Vec3 b1max_x b1max_y b1max_z = b1max
      small =
        point3 (min b0min_x b1min_x, min b0min_y b1min_y, min b0min_z b1min_z)
      big =
        point3 (max b0max_x b1max_x, max b0max_y b1max_y, max b0max_z b1max_z)
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
          _else -> return (h2, h1)
      _moreThanTwoHtbls -> do
        let (lt_htbls, rt_htbls) =
              S.splitAt (objectSpan `div` 2) (S.sortBy comparator htbls)
        lt <- makeBVH mtime lt_htbls
        rt <- makeBVH mtime rt_htbls
        return (lt, rt)
  let bvh_bx =
        surroundingBox (boundingBox bvh_lt mtime) (boundingBox bvh_rt mtime)
  return (BVHNode bvh_lt bvh_rt bvh_bx (S.length htbls))

boxCompare ::
     (Vec3 -> Double) -> Maybe (Time, Time) -> Hittable -> Hittable -> Ordering
boxCompare compAxis mtime boxa boxb =
  let (Box bmA _) = boundingBox boxa mtime
      (Box bmB _) = boundingBox boxb mtime
   in compare (compAxis bmA) (compAxis bmB)

hit :: Hittable -> Ray -> Double -> Double -> RandGen -> (Maybe Hit, RandGen)
hit (BVHNode bvh_l bvh_r box _) r t_min t_max gen =
  if boxRayIntersect box r t_min t_max
     -- try to hit left branch
    then case hit bvh_l r t_min t_max gen
           -- no hits, try right branch
               of
           (Nothing, g1) -> hit bvh_r r t_min t_max g1
            -- left branch hit
           (Just hitLeft@(Hit t _ _ _ _ _ _), g1)
             -- is there a closer right branch hit?
            ->
             case hit bvh_r r t_min t g1
                -- no, take hit from left branch
                   of
               (Nothing, g2)       -> (Just hitLeft, g2)
                -- yes, take hit from right branch
               (Just hitRight, g2) -> (Just hitRight, g2)
    else (Nothing, gen)
hit (Cuboid _ _ rl) r t_min t_max gen = foldr
  (\h acc@(_, g) -> closerHit (hit (Rect h) r t_min t_max g) acc)
  (Nothing, gen)
  rl
  where
    closerHit
      :: (Maybe Hit, RandGen) -> (Maybe Hit, RandGen) -> (Maybe Hit, RandGen)
    closerHit (Nothing, rg) (Nothing, _) = (Nothing, rg)
    closerHit (Just h1, rg) (Nothing, _) = (Just h1, rg)
    closerHit (Nothing, rg) (Just h2, _) = (Just h2, rg)
    closerHit
      (h1@(Just (Hit t1 _ _ _ _ _ _)), rg)
      (h2@(Just (Hit t2 _ _ _ _ _ _)), _) =
      if t1 < t2
      then (h1, rg)
      else (h2, rg)
hit (Rect rct) r@(Ray ror rdr _) t_min t_max gen =
  case rct of
    (XYRect x0 x1 y0 y1 k rmat) ->
      (rectHit x0 x1 y0 y1 vecX vecY vecZ (point3 (0, 0, 1)) k rmat, gen)
    (XZRect x0 x1 z0 z1 k rmat) ->
      (rectHit x0 x1 z0 z1 vecX vecZ vecY (point3 (0, 1, 0)) k rmat, gen)
    (YZRect y0 y1 z0 z1 k rmat) ->
      (rectHit y0 y1 z0 z1 vecY vecZ vecX (point3 (1, 0, 0)) k rmat, gen)
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
hit (Translate offset h) (Ray ror rdr tm) t_min t_max gen =
  let m_r = Ray (ror |-| offset) rdr tm
   in case hit h m_r t_min t_max gen of
        n@(Nothing, _) -> n
        (Just (Hit t p outwardNormal rec_u rec_v _ mat), g1) ->
          let (mFrontFace, mNormal) = faceNormal m_r outwardNormal
           in ( Just
                  (Hit t (p |+| offset) mNormal rec_u rec_v mFrontFace mat)
              , g1)
hit (Rotate axis sin_theta cos_theta _ h) (Ray ror rdr tm) t_min t_max gen =
  let rotated_r =
        Ray
          (unRotatePoint axis sin_theta cos_theta ror)
          (unRotatePoint axis sin_theta cos_theta rdr)
          tm
   in case hit h rotated_r t_min t_max gen of
        n@(Nothing, _) -> n
        (Just (Hit t p outwardNormal u v _ mat), g1) ->
          let rot_p = rotatePoint axis sin_theta cos_theta p
              rot_outwardNormal =
                rotatePoint axis sin_theta cos_theta outwardNormal
              (rot_frontFace, rot_normal) =
                faceNormal rotated_r rot_outwardNormal
           in (Just (Hit t rot_p rot_normal u v rot_frontFace mat), g1)
hit (ConstantMedium nInvD phFunc boundary) ray@(Ray _ rdr _) t_min t_max gen =
  case hit boundary ray (-infinity) infinity gen of
    n@(Nothing, _) -> n
    (Just (Hit h1t _ _ _ _ _ _), g1) ->
      case hit boundary ray (h1t + epsilon) infinity g1 of
        n@(Nothing, _) -> n
        (Just (Hit h2t _ _ _ _ _ _), g2) ->
          let rec1t' = max t_min h1t
              rec2t  = min t_max h2t
          in  if rec1t' >= rec2t
                then (Nothing, g2)
                else
                  let rec1t           = if rec1t' < 0 then 0 else rec1t'
                      rayLength       = Lib.length rdr
                      distInsideBound = (rec2t - rec1t) * rayLength
                      (rand, g3)      = randomDouble g2
                      hitDist         = nInvD * log rand
                  in  if hitDist > distInsideBound
                        then (Nothing, g3)
                        else
                          let
                            newt = rec1t + (hitDist / rayLength)
                            newp = ray `at` newt
                          in
                            ( Just
                              $ Hit newt newp (point3 (1, 0, 0)) 0 0 True phFunc
                            , g3
                            )
hit (Sphere sc sr sm) r@(Ray ror rdr _) t_min t_max gen =
  if discriminant > 0
    then let sd = sqrt discriminant
             temp1 = ((-b) - sd) / a
             temp2 = ((-b) + sd) / a
          in if | t_min < temp1 && temp1 < t_max -> (Just $ recHit temp1, gen)
                | t_min < temp2 && temp2 < t_max -> (Just $ recHit temp2, gen)
                | otherwise -> (Nothing, gen)
    else (Nothing, gen)
  where
    oc = ror |-| sc
    a = dot rdr rdr
    b = seq oc (dot oc rdr)
    c = seq sr (dot oc oc - (sr * sr))
    discriminant = b * b - a * c
    recHit :: Double -> Hit
    recHit temp =
      let p = r `at` temp
          pShift = p |-| sc
          outwardNormal = divide pShift sr
          (frontFace, normal) = faceNormal r outwardNormal
          phi = atan2 (vecZ outwardNormal) (vecX outwardNormal)
          theta = asin (vecY outwardNormal)
          (u, v) = (1.0 - ((phi + pi) / (2 * pi)), (theta + (pi / 2)) / pi)
       in Hit temp p normal u v frontFace sm
hit (MovingSphere c0 c1 t0 _ tp sr sm) r@(Ray _ _ t) t_min t_max gen =
  let sc = c0 |+| scale ((t - t0) / tp) (c1 |-| c0)
  in hit (Sphere sc sr sm) r t_min t_max gen
hit Unhittable _ _ _ gen = (Nothing, gen)

faceNormal :: Ray -> Point3 -> (Bool, Point3)
faceNormal (Ray _ rdr _) outwardNormal =
  let frontFace = rdr `dot` outwardNormal < 0
   in ( frontFace
      , if frontFace
          then outwardNormal
          else vecNegate outwardNormal)

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
  return $ point3 (x, y, z)

randomVec3DoubleRM :: Double -> Double -> RandomState s Vec3
randomVec3DoubleRM mn mx = do
  x <- randomDoubleRM mn mx
  y <- randomDoubleRM mn mx
  z <- randomDoubleRM mn mx
  return $ point3 (x, y, z)

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
      p = scale 2.0 (point3 (x, y, z)) |-| point3 (1.0, 1.0, 1.0)
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
      p = scale 2.0 (point3 (x, y, 0)) |-| point3 (1.0, 1.0, 0)
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
  return $ point3 (r * cos a, r * sin a, z)

randomInHemisphereM :: Point3 -> RandomState s Vec3
randomInHemisphereM n = do
  inUnitSphere <- randomInUnitSphereM
  if (inUnitSphere `dot` n) > 0.0
    then return inUnitSphere
    else return (vecNegate inUnitSphere)

randomCosineDirection :: RandomState s Vec3
randomCosineDirection = do
  gRef <- asks getGenRef
  g0 <- lift $ readSTRef gRef
  let (r1, g1) = randomDouble g0
  let (r2, g2) = randomDouble g1
  lift $ writeSTRef gRef g2
  let z = sqrt (1 - r2)
  let phi = 2 * pi * r1
  let x = cos phi * sqrt r2
  let y = sin phi * sqrt r2
  return $ point3 (x, y, z)

randomToSphereM :: Double -> Double -> RandomState s Vec3
randomToSphereM radius distSquared = do
  r1 <- randomDoubleM
  r2 <- randomDoubleM
  let z = 1 + r2 * (sqrt (1 - radius * radius / distSquared) - 1)
  let phi = 2 * pi * r1
  let oneMinusZSquared = sqrt (1 - z * z)
  let x = cos phi * oneMinusZSquared
  let y = sin phi * oneMinusZSquared
  return $ Vec3 x y z

data Camera
  = Camera
      !Point3
      -- ^  camera_origin
      !Point3
      -- ^  camera_llc
      !Point3
      -- ^  camera_horiz
      !Point3
      -- ^  camera_vert
      !Point3
      -- ^  camera_u
      !Point3
      -- ^  camera_v
      !Point3
      -- ^  _camera_w
      !Double
      -- ^  camera_lensRadius
      !Double
      -- ^  camera_t0
      !Double
      -- ^  camera_t1

getRay :: Camera -> Double -> Double -> RandomState s Ray
getRay (Camera c_or c_llc c_horiz c_vert c_u c_v _ c_lr c_time0 c_time1) s t =
  do
    rd <- fmap (scale c_lr) randomInUnitDiskM
    let offset = scale (vecX rd) c_u |+| scale (vecY rd) c_v
    tm <- randomDoubleRM c_time0 c_time1
    return $ Ray
      (c_or |+| offset)
      (        c_llc
      |+| scale s c_horiz
      |+| scale t c_vert
      |-| c_or
      |-| offset
      )
      tm

newCamera ::
     Point3
  -> Point3
  -> Point3
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
      w = makeUnitVector (lookfrom |-| lookat)
      u = makeUnitVector (cross vup w)
      v = cross w u
      lowerLeftCorner =
        origin |-| scale (halfWidth * focusDist) u |-|
        scale (halfHeight * focusDist) v |-|
        scale focusDist w
      horizontal = scale (2 * halfWidth * focusDist) u
      vertical = scale (2 * halfHeight * focusDist) v
   in Camera origin lowerLeftCorner horizontal vertical u v w lensRadius t0 t1

-- TODO: rewrite with foldM over [1..depth]?
rayColor  :: Ray -> Int -> RayTracingM s Albedo
rayColor ray depth = rayColorHelp ray depth id
  where
    rayColorHelp :: Ray -> Int -> (Albedo -> Albedo) -> RayTracingM s Albedo
    rayColorHelp r@(Ray _ _ rtime) d alb_acc =
      if d <= 0
      then return $ alb_acc (albedo (0.0, 0.0, 0.0))
      else do
        htbls <- asks getSceneHittables
        gRef <- asks getGenRef
        gen <- lift $ readSTRef gRef
        case hit htbls r epsilon infinity gen of
          (Nothing, g1) -> do
            bgd <- asks getBackground
            lift $ writeSTRef gRef g1
            return $ alb_acc bgd
          (Just h@(Hit _ hp hn hu hv _ hm), g1) -> do
            lift $ writeSTRef gRef g1
            mscatter <- scatter hm r h
            case mscatter of
              Nothing -> return $ alb_acc $ emitted hm r h hu hv hp
              Just (ScatterRecord newRay specular (Albedo att) pdfVal)
                -> if specular
                   then rayColorHelp
                     newRay
                     (d - 1)
                     (\(Albedo new)
                      -> alb_acc $ Albedo (att |*| new))
                   else rayColorHelp
                     newRay
                     (d - 1)
                     (\(Albedo new) -> alb_acc
                      $ Albedo
                        (att
                         |*| (scatteringPdf hm r h newRay
                              `scale` (new `divide` pdfVal))))

sampleColor :: Albedo -> (Double, Double) -> RayTracingM s Albedo
sampleColor (Albedo accCol) (u, v) = do
  camera <- asks getCamera
  r <- getRay camera u v
  maxDepth <- asks getMaxDepth
  (Albedo c1) <- rayColor r maxDepth
  return $ Albedo $ accCol |+| c1

renderPos :: RenderStaticEnv -> STRef s RandGen -> [(Double, Double)] -> ST s Albedo
renderPos staticEnv genRef samples =
  let ns = Prelude.length samples
   in runReaderT
        (do (Albedo summedColor) <-
              foldM sampleColor (albedo (0.0, 0.0, 0.0)) samples
            return $ Albedo $ divide summedColor (fromIntegral ns))
        (mkRenderEnv staticEnv genRef)

uniformRandomUVs
  :: Int
  -> Int
  -> Int
  -> (STRef s RandGen, (Int, Int))
  -> ST s [(Double, Double)]
uniformRandomUVs nsPerThread imageWidth imageHeight (gRef, (x, y)) = do
  gen <- readSTRef gRef
  let (gFin, res) = foldr
        (\_ (g, acc) ->
          let (ru, g1) = randomDouble g
              (rv, g2) = randomDouble g1
              u        = (fromIntegral x + ru) / fromIntegral imageWidth
              v        = (fromIntegral y + rv) / fromIntegral imageHeight
          in  (g2, (u, v) : acc)
        )
        (gen, [])
        [1 .. nsPerThread]
  writeSTRef gRef gFin
  return res

_testPoisson :: Int -> IO [(Double, Double)]
_testPoisson ns = do
  g <- newRandGen
  return $ runST $ do
    gRef        <- newSTRef g
    _poissonRandomUVs ns 1 1 (gRef, (0,0))

_poissonRandomUVs
  :: Int
  -> Int
  -> Int
  -> (STRef s RandGen, (Int, Int))
  -> ST s [(Double, Double)]
_poissonRandomUVs ns imageWidth imageHeight (gRef, (x, y)) = do
  g <- readSTRef gRef
  let (ru, g1) = randomDouble g
  let (rv, g2) = randomDouble g1
  let initialP = (xd + ru, yd + rv)
  let (result, newG) =
        go (M.singleton (getCoords initialP) 0, S.singleton initialP, [0], g2)
  writeSTRef gRef newG
  return $ map (\(sx, sy) -> (sx / imW, sy / imH)) result
  where
    k = 30 :: Int
    (imW, imH) = (fromIntegral imageWidth, fromIntegral imageHeight)
    (xd, yd) = (fromIntegral x, fromIntegral y)
    ns' = fromIntegral ns :: Double
    gridWidth = floor (sqrt ns') :: Int
    a = 1.0 / sqrt ns' :: Double
    r = sqrt 2 * a
    r2 = r * r
    go ::
         (M.Map (Int, Int) Int, Seq (Double, Double), [Int], RandGen)
      -> ([(Double, Double)], RandGen)
    go (_, samples, [], g) = (toList samples, g)
    go (cells, samples, active, g) =
      go $ foldr stepActive (cells, samples, [], g) active
    getCoords :: (Double, Double) -> (Int, Int)
    getCoords (dx, dy) =
      (floor ((dx - fromIntegral x) / a), floor ((dy - fromIntegral y) / a))
    stepActive ::
         Int
      -> (M.Map (Int, Int) Int, Seq (Double, Double), [Int], RandGen)
      -> (M.Map (Int, Int) Int, Seq (Double, Double), [Int], RandGen)
    stepActive curActive (cs, ss, newActives, g) =
      case nP of
        Nothing -> (cs, ss, newActives, g)
        Just p ->
          ( M.insert (getCoords p) (S.length ss) cs
          , ss S.|> p
          , S.length ss : curActive : newActives
          , newG)
      where
        (nP, newG) = getPoint (curX, curY) k g
        (curX, curY) = force $ S.index ss curActive

        getPoint ::
             (Double, Double)
          -> Int
          -> RandGen
          -> (Maybe (Double, Double), RandGen)
        getPoint _ 0 g' = (Nothing, g')
        getPoint curPos kRem g' =
          let ((npx, npy), g1) = newRandomPoint curPos g'
           in if and [npx > xd, npx < xd + 1, npy > yd, npy < yd + 1] &&
                 pointValid (npx, npy)
                then (Just (npx, npy), g1)
                else getPoint curPos (kRem - 1) g1

        newRandomPoint ::
             (Double, Double) -> RandGen -> ((Double, Double), RandGen)
        newRandomPoint (cpx, cpy) gen =
          let (rho, g1) = randomDoubleR (r, 2 * r) gen
              (theta, g2) = randomDoubleR (0, 2 * pi) g1
           in ((cpx + rho * cos theta, cpy + rho * sin theta), g2)

        pointValid :: (Double, Double) -> Bool
        pointValid (npx, npy) =
          all
            (\(nbx, nby) -> (nbx - npx) ** 2 + (nby - npy) ** 2 >= r2)
            (getNeighbors (getCoords (npx, npy)))

        getNeighbors :: (Int, Int) -> [(Double, Double)]
        getNeighbors (x', y') =
          map (force $ S.index ss . (M.!) cs) $
          filter
            (\(nx, ny) ->
               and [nx >= 0, nx < gridWidth, ny >= 0, ny < gridWidth] &&
               M.member (nx, ny) cs)
            (map (\(dx, dy) -> (x' + dx, y' + dy)) dxdy)
          where
            dxdy =
              [ (-1, -2)
              , (0, -2)
              , (1, -2)
              , (-2, -1)
              , (-1, -1)
              , (0, -1)
              , (1, -1)
              , (2, -1)
              , (-2, 0)
              , (-1, 0)
              , (1, 0)
              , (2, 0)
              , (-2, 1)
              , (-1, 1)
              , (0, 1)
              , (1, 1)
              , (2, 1)
              , (-1, 2)
              , (0, 2)
              , (1, 2)
              , (0, 0)
              ] :: [(Int, Int)]

pixelPositions :: Int -> Int -> [VV.Vector (Int, Int)]
pixelPositions nx ny = map (\y -> VV.fromList $ map (, y) [0 .. nx - 1]) [ny - 1,ny - 2 .. 0]

runRender :: RenderStaticEnv -> [RandGen] -> [VV.Vector RGB]
runRender staticEnv gens =
  let imageWidth = getStaticImageWidth staticEnv
      imageHeight = getStaticImageHeight staticEnv
      ns = getNumSamples (mkRenderEnv staticEnv undefined)
      pp = pixelPositions imageWidth imageHeight
   in runST $ do
        gensRef <- newSTRef (VV.fromList gens)
        mapM
          (\row -> do
             gs <- readSTRef gensRef
             let (renderedRow, newGs) =
                   VV.unzip
                     (VV.map
                        (\(g, pos) ->
                           force $
                           runST $ do
                             gRef <- newSTRef g
                             uvs <-
                               uniformRandomUVs
                                 ns
                                 imageWidth
                                 imageHeight
                                 (gRef, pos)
                             renderedPos <-
                               fmap albedoToColor (renderPos staticEnv gRef uvs)
                             ng <- readSTRef gRef
                             return (renderedPos, ng))
                        (VV.zip gs row) `using`
                      parTraversable rpar)
             writeSTRef gensRef newGs
             return renderedRow)
          pp
