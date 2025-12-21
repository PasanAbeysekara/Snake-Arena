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
-- Adjusted for asymmetric brick walls (top has HUD space)
gridToScreen :: Position -> (Float, Float)
gridToScreen (gx, gy) =
  ( fromIntegral gx * fromIntegral cellPixelSize
  , fromIntegral gy * fromIntegral cellPixelSize - 30  -- Offset by 30 pixels to center vertically
  )

-- | Get a random position within grid bounds
-- Excludes wall boundaries (2 cells margin on each side)
randomPos :: Int -> Int -> StdGen -> (Position, StdGen)
randomPos w h gen =
  let margin = 2  -- Keep 2 cells away from walls
      (rx, gen1) = randomR (-w `div` 2 + margin, w `div` 2 - margin - 1) gen
      (ry, gen2) = randomR (-h `div` 2 + margin, h `div` 2 - margin - 1) gen1
  in ((rx, ry), gen2)

-- | Check if a position is inside the grid
inBounds :: Int -> Int -> Position -> Bool
inBounds w h (x, y) =
  let halfW = w `div` 2
      halfH = h `div` 2
  in x > -halfW && x < halfW &&
     y >= -halfH && y < halfH

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
