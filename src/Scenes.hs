{-# LANGUAGE MultiWayIf #-}
module Scenes
  ( cornellCamera,
    earthTexture,
    makeCornellBoxScene,
    makeCornellSmokeBoxScene,
    makeEarthScene,
    makeNextWeekFinalScene,
    makeRandomScene,
    makeRandomSceneBookOne,
    makeSimpleLightScene,
    makeTwoPerlinSpheresScene,
    makeTwoSpheresScene,
    mkRenderStaticEnv,
    nextWeekFinalSceneCamera,
    randomSceneCamera,
    twoSpheresSceneCamera,
  )
where

import qualified Codec.Picture         as JP (Image (..), convertRGB8,
                                              imageHeight, imageWidth,
                                              readImage)
import           Control.Monad.Reader
import           Control.Monad.ST.Lazy (runST)
import           Data.Maybe            (catMaybes)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as S
import           Data.STRef.Lazy
import           Lib

makeCornellBoxScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeCornellBoxScene t0 t1 gen = runST $ do
  gRef <- newSTRef gen
  world <-
    runReaderT
      ( do
          let red = Lambertian $ ConstantColor (albedo (0.65, 0.05, 0.05))
          let white = Lambertian $ ConstantColor (albedo (0.73, 0.73, 0.73))
          let green = Lambertian $ ConstantColor (albedo (0.12, 0.45, 0.15))
          let light = DiffuseLight $ ConstantColor (albedo (15, 15, 15))
          makeBVH
            (Just (t0, t1))
            ( rect YZPlane 0 555 0 555 555 green
                :<| rect YZPlane 0 555 0 555 0 red
                :<| rect XZPlane 213 343 227 332 554 light
                :<| rect XZPlane 0 555 0 555 0 white
                :<| rect XZPlane 0 555 0 555 555 white
                :<| rect XYPlane 0 555 0 555 555 white
                :<| translate
                  (point3 (265, 0, 295))
                  ( rotate
                      YAxis
                      15
                      (cuboid (point3 (0, 0, 0)) (point3 (165, 330, 165)) white)
                  )
                :<| translate
                  (point3 (130, 0, 65))
                  ( rotate YAxis (-18) $
                      rotate
                        ZAxis
                        30
                        (cuboid (point3 (0, 0, 0)) (point3 (165, 165, 165)) white)
                  )
                :<| Empty
            )
      )
      (dummyRenderEnv gRef)
  g1 <- readSTRef gRef
  return ((world, albedo (0.0, 0.0, 0.0)), g1)

makeCornellSmokeBoxScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeCornellSmokeBoxScene t0 t1 gen = runST $ do
  gRef <- newSTRef gen
  world <-
    runReaderT
      ( do
          let red = Lambertian $ ConstantColor (albedo (0.65, 0.05, 0.05))
          let white = Lambertian $ ConstantColor (albedo (0.73, 0.73, 0.73))
          let green = Lambertian $ ConstantColor (albedo (0.12, 0.45, 0.15))
          let light = DiffuseLight $ ConstantColor (albedo (7, 7, 7))
          makeBVH
            (Just (t0, t1))
            ( rect YZPlane 0 555 0 555 555 green
                :<| rect YZPlane 0 555 0 555 0 red
                :<| rect XZPlane 113 443 127 432 554 light
                :<| rect XZPlane 0 555 0 555 0 white
                :<| rect XZPlane 0 555 0 555 555 white
                :<| rect XYPlane 0 555 0 555 555 white
                :<| constantMedium
                  0.01
                  (ConstantColor (albedo (0, 0, 0)))
                  ( translate
                      (point3 (265, 0, 295))
                      ( rotate
                          YAxis
                          15
                          (cuboid (point3 (0, 0, 0)) (point3 (165, 330, 165)) white)
                      )
                  )
                :<| constantMedium
                  0.01
                  (ConstantColor (albedo (1, 1, 1)))
                  ( translate
                      (point3 (130, 0, 65))
                      ( rotate YAxis (-18) (cuboid (point3 (0, 0, 0)) (point3 (165, 165, 165)) white)
                      )
                  )
                :<| Empty
            )
      )
      (dummyRenderEnv gRef)
  g1 <- readSTRef gRef
  return ((world, albedo (0.0, 0.0, 0.0)), g1)

cornellCamera :: (Int, Int) -> Camera
cornellCamera (imageWidth, imageHeight) =
  newCamera
    (point3 (278, 278, -800))
    (point3 (278, 278, 0.0))
    (point3 (0.0, 1.0, 0.0))
    40.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    10.0
    0.0
    1.0

makeSimpleLightScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeSimpleLightScene t0 t1 gen = runST $ do
  gRef <- newSTRef gen
  world <-
    runReaderT
      ( do
          perText <- makePerlin 1.0
          let difflight = DiffuseLight $ ConstantColor $ albedo (4, 4, 4)
          makeBVH
            (Just (t0, t1))
            ( sphere (point3 (0, -1000, 0)) 1000 (Lambertian perText)
                --               :<| Sphere (Vec3 (0, 2, 0)) 2 (Lambertian (ConstantColor $ albedo (0.5, 0.0, 0.3)))
                :<| sphere (point3 (0, 2, 0)) 2 (Lambertian perText)
                :<| sphere (point3 (0, 7, 0)) 2 difflight
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
        Left _ -> (Nothing, 0, 0)
        Right x ->
          let im = JP.convertRGB8 x
           in (Just (Image im), JP.imageWidth im, JP.imageHeight im)
  return $ ImageTexture earthIm w h

makeEarthScene :: Texture -> Time -> Time -> RandGen -> (Scene, RandGen)
makeEarthScene earthTex t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    world <-
      runReaderT
        ( makeBVH
            (Just (t0, t1))
            (Sphere (point3 (0, 0, 0)) 2 (Lambertian earthTex) :<| Empty)
        )
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (1.00, 1.00, 1.00)), g1)

twoSpheresSceneCamera :: (Int, Int) -> Camera
twoSpheresSceneCamera (imageWidth, imageHeight) =
  newCamera
    (point3 (26.0, 4.0, 6.0))
    (point3 (0.0, 2.0, 0.0))
    (point3 (0.0, 1.0, 0.0))
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
        ( do
            perText <- makePerlin 1.5
            makeBVH
              (Just (t0, t1))
              ( Sphere (point3 (0, -1000, 0)) 1000 (Lambertian perText)
                  :<| Sphere (point3 (0, 2, 0)) 2 (Lambertian perText)
                  :<| Empty
              )
        )
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0, 0, 0)), g1)

makeTwoSpheresScene :: Time -> Time -> RandGen -> (Scene, RandGen)
makeTwoSpheresScene t0 t1 gen =
  runST $ do
    let checkerMaterial =
          Metal
            ( CheckerTexture
                (ConstantColor $ albedo (0.2, 0.3, 0.1))
                (ConstantColor $ albedo (0.9, 0.9, 0.9))
            )
            (Fuzz 0.0)
    let flatMaterial =
          Lambertian (ConstantColor $ albedo (0.6, 0.2, 0.1))
    gRef <- newSTRef gen
    world <-
      runReaderT
        ( makeBVH
            (Just (t0, t1))
            ( Sphere (point3 (0, -10, 0)) 10 checkerMaterial
                :<| Sphere (point3 (0, 10, 0)) 10 flatMaterial
                :<| Empty
            )
        )
        (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0.8, 0.8, 0.9)), g1)

randomSceneCamera :: (Int, Int) -> Camera
randomSceneCamera (imageWidth, imageHeight) =
  newCamera
    (point3 (13.0, 2.0, 3.0))
    (point3 (0.0, 0.0, 0.0))
    (point3 (0.0, 1.0, 0.0))
    20.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    10.0
    0.0
    1.0

-- | Generate the image from the cover of the book one with lots of spheres
makeRandomSceneBookOne :: RandGen -> (Scene, RandGen)
makeRandomSceneBookOne gen = runST $ do
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
              (point3 (0.0, -1000.0, 0.0))
              1000
              (Lambertian (ConstantColor $ albedo (0.5, 0.5, 0.5))) --gray
      let s1 =
            sphere (point3 (0.0, 1.0, 0.0)) 1.0 (Dielectric (RefractiveIdx 1.5))
      let s2 =
            sphere
              (point3 (-4.0, 1.0, 0.0))
              1.0
              (Lambertian (ConstantColor $ albedo (0.4, 0.2, 0.1)))
      let s3 =
            sphere
              (point3 (4.0, 1.0, 0.0))
              1.0
              (Metal (ConstantColor $ albedo (0.7, 0.6, 0.5)) (Fuzz 0.0))
      nps <- catMaybes <$> mapM makeRandomSphereM ns
      world <-
        makeBVH (Just (0.0, 1.0)) $ ground :<| s1 :<| s2 :<| s3 :<| S.fromList nps
      return (world, albedo (0.7, 0.8, 0.9))
    makeRandomSphereM :: (Int, Int) -> RandomState s (Maybe Hittable)
    makeRandomSphereM (a, b) = do
      mat <- randomDoubleM
      px <- randomDoubleM
      py <- randomDoubleM
      let center =
            point3 (fromIntegral a + 0.9 * px, 0.2, fromIntegral b + 0.9 * py)
      if Lib.length (center `vecSub` point3 (4.0, 0.2, 0)) <= 0.9
        then return Nothing
        else
          if
              | mat < 0.8 -> -- Diffuse
                do
                  a1 <- randomVec3DoubleM
                  a2 <- randomVec3DoubleM
                  let alb = Albedo $ a1 `vecMul` a2
                  return $ Just $ sphere center 0.2 (Lambertian (ConstantColor alb))
              | mat < 0.95 -> -- Metal
                do
                  alb <- Albedo <$> randomVec3DoubleRM 0.5 1.0
                  fuzz <- randomDoubleRM 0.0 0.5
                  return $
                    Just $
                      sphere
                        center
                        0.2
                        (Metal (ConstantColor alb) (Fuzz fuzz))
              | otherwise -> --Glass
                return $
                  Just $
                    sphere
                      center
                      0.2
                      (Dielectric (RefractiveIdx 1.5))

-- | Generate the image from the cover of the book with lots of
--  spheres, but tweaked to demonstrate other features
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
              (point3 (0.0, -1000.0, 0.0))
              1000
              -- (Lambertian (ConstantColor $ albedo (0.5, 0.5, 0.5))) --gray
              ( Lambertian
                  ( CheckerTexture
                      (ConstantColor $ albedo (0.2, 0.3, 0.1))
                      (ConstantColor $ albedo (0.9, 0.9, 0.9))
                  )
              )
      let s1 =
            -- Sphere (Vec3 (0.0, 1.0, 0.0)) 1.0 (Dielectric (RefractiveIdx 1.5))
            cuboid
              (point3 (-0.75, 0.0, -0.75))
              (point3 (0.75, 1.5, 0.75))
              (Dielectric (RefractiveIdx 1.5))
      let s2 =
            sphere
              (point3 (-4.0, 1.0, 0.0))
              1.0
              -- (Lambertian (ConstantColor $ albedo (0.4, 0.2, 0.1)))
              (Lambertian earthtex)
      let s3 =
            sphere
              (point3 (4.0, 1.0, 0.0))
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
            point3 (fromIntegral a + 0.9 * px, 0.2, fromIntegral b + 0.9 * py)
      if Lib.length (center `vecSub` point3 (4.0, 0.2, 0)) <= 0.9
        then return Nothing
        else
          if
              | mat < 0.8 -> -- Diffuse
                do
                  a1 <- randomVec3DoubleM
                  a2 <- randomVec3DoubleM
                  sph_move_x <- randomDoubleRM (-0.25) 0.25
                  sph_move_z <- randomDoubleRM (-0.25) 0.25
                  let alb = Albedo $ a1 `vecMul` a2
                  return $ Just $ sphere center 0.2 (Dielectric (RefractiveIdx 1.5))

nextWeekFinalSceneCamera :: (Int, Int) -> Camera
nextWeekFinalSceneCamera (imageWidth, imageHeight) =
  newCamera
    (point3 (575, 278, -525))
    (point3 (320, 278, 0.0))
    (point3 (0.0, 1.0, 0.0))
    40.0
    (fromIntegral imageWidth / fromIntegral imageHeight)
    0.1
    580.0
    0.0
    1.0

makeNextWeekFinalScene :: Texture -> Time -> Time -> RandGen -> (Scene, RandGen)
makeNextWeekFinalScene earthtex t0 t1 gen =
  runST $ do
    gRef <- newSTRef gen
    world <- runReaderT makeSceneM (dummyRenderEnv gRef)
    g1 <- readSTRef gRef
    return ((world, albedo (0, 0, 0)), g1)
  where
    makeSceneM :: RandomState s Hittable
    makeSceneM = do
      let ground = Lambertian (ConstantColor $ albedo (0.48, 0.83, 0.53))
      let white = Lambertian $ ConstantColor (albedo (0.73, 0.73, 0.73))
      let w = 100 :: Double
      let y0 = 0 :: Double
      boxes1 <-
        makeBVH (Just (0, 1)) =<< S.fromList
          <$> mapM
            ( \(i, j) -> do
                let x0 = i * w - 1000
                let z0 = j * w - 1000
                let x1 = x0 + w
                y1 <- randomDoubleRM 1 101
                let z1 = z0 + w
                return $ cuboid (point3 (x0, y0, z0)) (point3 (x1, y1, z1)) ground
            )
            [(i, j) | i <- [0 .. 19], j <- [0 .. 19]]
      let light = DiffuseLight $ ConstantColor (albedo (7, 7, 7))
      let boundary1 = sphere (point3 (360, 150, 145)) 70 (Dielectric $ RefractiveIdx 1.5)
      let boundary2 = sphere (point3 (0, 0, 0)) 5000 (Dielectric $ RefractiveIdx 1.5)
      pertext <- makePerlin 0.1
      boxes2 <-
        makeBVH (Just (0, 1)) =<< S.fromList
          <$> replicateM
            1000
            ( do
                randP <- randomVec3DoubleRM 0 165
                return $ sphere randP 10 white
            )
      makeBVH
        (Just (t0, t1))
        ( boxes1
            :<| rect XZPlane 113 443 127 432 554 light
            :<| movingSphere (point3 (400, 400, 200)) (point3 (430, 400, 200)) t0 t1 50 (Lambertian (ConstantColor $ albedo (0.7, 0.3, 0.1)))
            :<| sphere (point3 (260, 150, 45)) 50 (Dielectric $ RefractiveIdx 1.5)
            :<| sphere (point3 (0, 150, 145)) 50 (Metal (ConstantColor $ albedo (0.8, 0.8, 0.9)) (Fuzz 10.0))
            :<| boundary1
            :<| constantMedium 0.2 (ConstantColor $ albedo (0.2, 0.4, 0.9)) boundary1
            :<| constantMedium 0.0001 (ConstantColor $ albedo (1, 1, 1)) boundary2
            :<| sphere (point3 (400, 200, 400)) 100 (Lambertian earthtex)
            :<| sphere (point3 (220, 280, 300)) 80 (Lambertian pertext)
            :<| translate (point3 (-100, 270, 395)) (rotate YAxis 15 boxes2)
            :<| Empty
        )
