module Main where

import           Control.Monad (replicateM)
import           Lib
import           Scenes
import           System.IO     (hPutStr, stderr, stdout)

-- X and Y dimensions of output image
defaultImageWidth :: Int
defaultImageWidth = 384
defaultImageHeight :: Int
defaultImageHeight = 216

-- Number of threads to use when rendering
defaultnThreads :: Int
defaultnThreads = 2

-- Number of samples to use when anti-aliasing
defaultNs :: Int
defaultNs = 100

-- maximum number of reflections
defaultMaxDepth :: Int
defaultMaxDepth = 50

main :: IO ()
main = do
  let imageWidth = defaultImageWidth
  let imageHeight = defaultImageHeight
  let gen = randGen 1024 -- Fix a seed for comparable performance tests
  et <- earthTexture
  let camera = randomSceneCamera (imageWidth, imageHeight)
  let (world, g1) = makeRandomSceneBookOne gen
  --let (world, g1) = makeRandomScene et 0.0 1.0 gen
  --let camera = twoSpheresSceneCamera (imageWidth, imageHeight)
  --let (world, g1) = makeEarthScene et 0.0 1.0 gen
  --let (world, g1) = makeTwoPerlinSpheresScene 0.0 1.0 gen
  --let (world, g1) = makeTwoSpheresScene 0.0 1.0 gen
  --let (world, g1) = makeSimpleLightScene 0.0 1.0 gen
  --let camera = cornellCamera (imageWidth, imageHeight)
  --let (world, g1) = makeCornellBoxScene 0.0 1.0 gen
  --let (world, g1) = makeCornellSmokeBoxScene 0.0 1.0 gen
  -- let imageWidth = 500
  -- let imageHeight = 500
  -- let camera = nextWeekFinalSceneCamera (imageWidth, imageHeight)
  -- let (world, g1) = makeNextWeekFinalScene et 0.0 1.0 gen
  -- gs <- replicateM (defaultnThreads - 1) newRandGen
  gs <- replicateM (defaultImageWidth - 1) newRandGen
  let gens = g1 : gs
  let staticEnv =
        mkRenderStaticEnv
          world
          camera
          (imageWidth, imageHeight)
          defaultNs
          defaultMaxDepth
          defaultnThreads
  let vals = runRender staticEnv gens
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  mapM_ (printRow stdout imageHeight) (zip [1 .. imageHeight] vals)
  hPutStr stderr "\nDone.\n"
