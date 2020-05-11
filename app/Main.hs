module Main where

import           Lib
import           System.IO (hPutStr, stderr)

-- X and Y dimensions of output image
defaultImageWidth :: Int
defaultImageWidth = 600
defaultImageHeight :: Int
defaultImageHeight = 400

-- Number of threads to use when rendering
defaultnThreads :: Int
defaultnThreads = 2

-- Number of samples to use when anti-aliasing
defaultNs :: Int
defaultNs = 100

-- nsPerThread :: Int
-- nsPerThread = ns `div` nThreads

-- maximum number of reflections
defaultMaxDepth :: Int
defaultMaxDepth = 50

main :: IO ()
main = do
  let imageWidth = defaultImageWidth
  let imageHeight = defaultImageHeight
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  let gen = pureMT 1024 -- Fix a seed for comparable performance tests
  let (world, g1) = makeRandomScene 0.0 1.0 gen
  -- (world, g1) = makeTwoPerlinSpheresScene 0.0 1.0 gen
  let camera = randomSceneCamera (imageWidth, imageHeight)
  let staticEnv =
        mkRenderStaticEnv
          world
          camera
          (imageWidth, imageHeight)
          defaultNs
          defaultMaxDepth
          defaultnThreads
  let vals = runRender staticEnv g1
  mapM_ (printRow imageHeight) (zip [1 .. imageHeight] vals)
  hPutStr stderr "\nDone.\n"
