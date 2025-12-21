module DataTypes where

import System.Random (StdGen)
import Graphics.Gloss.Interface.Pure.Game (Picture)

-- | Directions the snake can move
data Direction = U | D | L | R
  deriving (Show, Eq, Read)

-- | Soldier that shoots bullets
data Soldier = Soldier
  { soldierPos :: Position
  , soldierDir :: Direction
  , shootTimer :: Float  -- Time until next shot
  } deriving (Show, Eq)

-- | Bullet fired by soldiers
data Bullet = Bullet
  { bulletPos :: Position
  , bulletDir :: Direction
  , bulletSpeed :: Float  -- Movement accumulator
  } deriving (Show, Eq)

-- | Types of power-ups available
data PowerType = SpeedBoost | ScoreMultiplier | Heart
  deriving (Show, Eq)

-- | Difficulty levels
data Difficulty = Easy | Normal | Hard
  deriving (Show, Eq, Read)

-- | Coordinates on the grid (x, y)
type Position = (Int, Int)

-- | Snake is a list of positions, head first
type Snake = [Position]

-- | Possible game outcomes
data Outcome = Menu | Running | Paused | GameOver
  deriving (Show, Eq)

-- | Core Game State
data GameState = GameState
  { snake       :: Snake
  , dir         :: Direction
  , nextDir     :: Direction      -- Buffer for the next tick's direction to prevent 180 turns
  , foodPos     :: Position
  , powerPos    :: Maybe (PowerType, Position)
  , score       :: Int
  , hiScore     :: Int
  , gridWidth   :: Int
  , gridHeight  :: Int
  , status      :: Outcome
  , rng         :: StdGen         -- Pure random number generator
  , timeSinceMove :: Float
  , curSpeed    :: Float          -- Seconds per move (lower is faster)
  , powerTimer  :: Float          -- Time remaining for active power-up
  , activePower :: Maybe PowerType
  , moveHistory :: [Direction]    -- History of moves for replay
  , obstacles   :: [Position]     -- Obstacle positions that snake cannot pass
  , difficulty  :: Difficulty     -- Current difficulty level
  , health      :: Int            -- Snake's armor health (max 10)
  , soldiers    :: [Soldier]      -- Active soldiers on the field
  , bullets     :: [Bullet]       -- Active bullets in the air
  }

-- | Replay data structure for storage
data Replay = Replay
  { rMoves :: [Direction]         -- This might need to be (Time, Direction) or just sequence of inputs
  , rFinalScore :: Int
  , rDuration :: Float
  } deriving (Show, Read)
