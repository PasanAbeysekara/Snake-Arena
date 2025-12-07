module Utils where

import DataTypes
import System.Random (StdGen, randomR)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)

-- | Window / Screen constants
windowWidth, windowHeight :: Int
windowWidth = 1200
windowHeight = 800

cellPixelSize :: Int
cellPixelSize = 20

-- | Convert grid position to screen coordinates (center of the cell)
gridToScreen :: Position -> (Float, Float)
gridToScreen (gx, gy) =
  ( fromIntegral gx * fromIntegral cellPixelSize
  , fromIntegral gy * fromIntegral cellPixelSize
  )

-- | Get a random position within grid bounds
randomPos :: Int -> Int -> StdGen -> (Position, StdGen)
randomPos w h gen =
  let (rx, gen1) = randomR (-w `div` 2, w `div` 2 - 1) gen
      (ry, gen2) = randomR (-h `div` 2, h `div` 2 - 1) gen1
  in ((rx, ry), gen2)

-- | Check if a position is inside the grid
inBounds :: Int -> Int -> Position -> Bool
inBounds w h (x, y) =
  x >= -w `div` 2 && x < w `div` 2 &&
  y >= -h `div` 2 && y < h `div` 2

-- | Analytics
aggregateStats :: [Replay] -> String
aggregateStats [] = "No replays found."
aggregateStats rs = unlines
  [ "Total Games: " ++ show total
  , "Average Score: " ++ show avgScore
  , "Best Score: " ++ show maxScore
  , "Average Duration: " ++ show avgDur ++ "s"
  ]
  where
    total = length rs
    scores = map rFinalScore rs
    durs = map rDuration rs
    sumScore = sum scores
    avgScore = fromIntegral sumScore / fromIntegral total :: Float
    maxScore = maximum scores
    avgDur = sum durs / fromIntegral total
