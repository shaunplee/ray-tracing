module Random
  ( newRandGen,
    randomDouble,
    randomDoubleR,
    randGen,
    RandGen (..),
  )
where

import           Control.DeepSeq (NFData, rnf)
import qualified System.Random   as Random

newtype RandGen = RandGen Random.StdGen
  deriving (Show)

instance NFData RandGen where
  rnf (RandGen pmt) = seq pmt ()

newRandGen :: IO RandGen
newRandGen = RandGen <$> Random.newStdGen

randGen :: Int -> RandGen
randGen s = RandGen (Random.mkStdGen s)

randomDouble :: RandGen -> (Double, RandGen)
randomDouble (RandGen g) =
  let (x, g1) = Random.random g
   in (x, RandGen g1)

randomDoubleR :: (Double, Double) -> RandGen -> (Double, RandGen)
randomDoubleR range (RandGen g) =
  let (res, newGen) = Random.uniformR range g
   in (res, RandGen newGen)
