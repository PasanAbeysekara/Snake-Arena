# Architecture Documentation

## Overview
Snake Arena is designed using the **Pure Core, Thin Shell** architectural pattern, a functional programming approach that strictly separates pure business logic from side effects. This architecture ensures predictability, testability, and maintainability.

## Architectural Principles

### 1. Pure Core, Thin Shell Pattern
The application is divided into two distinct layers:

#### Pure Core (Pure Functions)
- **Location**: `Processing.hs`, `DataTypes.hs`, `Utils.hs`
- **Characteristics**:
  - All functions are pure (no side effects)
  - Deterministic behavior (same input → same output)
  - Immutable data structures
  - Testable without mocking or complex setup

#### Thin Shell (IO Layer)
- **Location**: `Main.hs`, `IOHandler.hs`
- **Characteristics**:
  - Handles all side effects (rendering, file I/O, user input)
  - Minimal logic, primarily orchestration
  - Delegates computation to pure core

### 2. Unidirectional Data Flow

```
User Input → GameState Update → Pure Transformations → New GameState → Render
     ↑                                                                    ↓
     └────────────────────── Game Loop ─────────────────────────────────┘
```

## Module Architecture

### Core Modules

#### `DataTypes.hs`
**Purpose**: Define all domain types using Algebraic Data Types (ADTs)

**Key Types**:
- `GameState`: The complete immutable game state
  - Contains: snake position, direction, score, obstacles, bullets, etc.
  - Immutable record that gets transformed, never mutated
  
- `Direction`: Sum type for movement (U | D | L | R)
- `Outcome`: Game status (Menu | Running | Paused | GameOver)
- `PowerType`: Power-up variants (SpeedBoost | ScoreMultiplier | Heart)
- `Difficulty`: Difficulty levels (Easy | Normal | Hard)
- `Soldier`, `Bullet`: Entity types for gameplay elements
- `Replay`: Serializable game session data

**FP Principles Applied**:
- **Sum Types**: Make invalid states unrepresentable
- **Product Types**: Group related data logically
- **Type Safety**: Compiler prevents many runtime errors

#### `Processing.hs`
**Purpose**: Pure game logic engine

**Key Functions**:

```haskell
-- Initialize game state with pure random seed
initialState :: Int -> Int -> StdGen -> Difficulty -> GameState

-- Handle direction changes (pure, no side effects)
handleInput :: Direction -> GameState -> GameState

-- Main game loop update (pure time-based transformation)
step :: Float -> GameState -> GameState

-- Advance game by one tick
advanceGameState :: GameState -> GameState

-- Spawn game entities (pure, using StdGen for randomness)
spawnItems :: Bool -> Bool -> GameState -> (StdGen, Position, Maybe (PowerType, Position))
generateObstacles :: GameState -> Int -> StdGen -> [Position]
spawnSoldiers :: GameState -> Int -> StdGen -> ([Soldier], StdGen)
```

**FP Principles Applied**:
- **Pure Functions**: All transformations are deterministic
- **Referential Transparency**: Functions can be replaced with their return values
- **Function Composition**: Complex behavior emerges from simple function composition
- **Deterministic Randomness**: Uses `StdGen` threaded through state for pure randomness

#### `Utils.hs`
**Purpose**: Helper functions for common operations

**Key Functions**:
```haskell
-- Coordinate transformations
gridToScreen :: Position -> (Float, Float)

-- Boundary checking
inBounds :: Int -> Int -> Position -> Bool

-- Random position generation (pure)
randomPos :: Int -> Int -> StdGen -> (Position, StdGen)

-- Analytics aggregation (uses fold, map, filter)
aggregateStats :: [Replay] -> String
```

**FP Principles Applied**:
- **Pure Transformations**: No side effects in utility functions
- **Higher-Order Functions**: Analytics use `map`, `filter`, `fold`
- **Lazy Evaluation**: Efficient processing of potentially large datasets

### IO Shell Modules

#### `Main.hs`
**Purpose**: Application entry point and rendering

**Responsibilities**:
- Initialize game state with IO-generated random seed
- Coordinate with Gloss library for rendering
- Translate IO events to pure function calls
- Manage game loop using `playIO`

**Architecture Pattern**:
```haskell
main :: IO ()
main = do
  gen <- newStdGen  -- Get random seed from IO
  savedHighScore <- loadHighScore  -- Load persisted data
  let initState = (initialState 56 33 gen Normal) { hiScore = savedHighScore }
  playIO window background 60 initState renderIO handleInputIO updateIO

-- Render wrapper (IO → Pure)
renderIO :: GameState -> IO Picture
renderIO gs = return $ renderPure gs

-- Pure rendering function
renderPure :: GameState -> Picture
```

**Key Design**:
- IO happens only at boundaries
- Pure functions handle all rendering logic
- State transformations remain pure

#### `IOHandler.hs`
**Purpose**: File I/O operations for persistence

**Key Functions**:
```haskell
saveReplay :: Replay -> IO ()
loadReplay :: FilePath -> IO (Either String Replay)
saveHighScore :: Int -> IO ()
loadHighScore :: IO Int
```

**FP Principles Applied**:
- **Effect Isolation**: All file I/O isolated in one module
- **Pure Data Structures**: `Replay` type is pure, serialization is effect
- **Error Handling**: Uses `Either` for functional error handling

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────┐
│                  Main.hs (IO)                       │
│  - User Input Events                                │
│  - Random Seed Generation                           │
│  - High Score Loading                               │
└────────────────────┬────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────┐
│              Processing.hs (Pure Core)              │
│                                                     │
│        ┌─────────────┐      ┌──────────────┐        │
│        │   step()    │ ───→ │ handleInput()│        │
│        └──────┬──────┘      └──────────────┘        │
│               │                                     │
│               ↓                                     │
│        ┌─────────────────────┐                      │
│        │ advanceGameState()  │                      │
│        └──────┬──────────────┘                      │
│               │                                     │
│               ├─→ spawnItems()                      │
│               ├─→ generateObstacles()               │
│               ├─→ spawnSoldiers()                   │
│               └─→ collision detection               │
│                                                     │
└────────────────────┬────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────┐
│                  DataTypes.hs                       │
│  - GameState (immutable)                            │
│  - ADTs (Direction, Outcome, PowerType, etc.)       │
└─────────────────────────────────────────────────────┘
                     │
                     ↓
┌─────────────────────────────────────────────────────┐
│               Utils.hs (Pure Helpers)               │
│  - Coordinate transforms                            │
│  - Boundary checks                                  │
│  - Analytics aggregation                            │
└─────────────────────────────────────────────────────┘
                     │
                     ↓
┌──────────────────────────────────────────────────────┐
│            IOHandler.hs (IO Persistence)             │
│  - Save/Load Replays                                 │
│  - Save/Load High Scores                             │
└──────────────────────────────────────────────────────┘
```

## State Management

### Immutable State Updates

The `GameState` record is never mutated. Instead, each update produces a new instance:

```haskell
-- Old state → Transformation → New state
step :: Float -> GameState -> GameState
step dt gs = 
  let newTime = timeSinceMove gs + dt
      gs' = gs { timeSinceMove = newTime }  -- Create new state
  in if newTime >= curSpeed gs
     then advanceGameState gs'
     else gs'
```

### Pure Randomness

Randomness is achieved without side effects by threading `StdGen` through the state:

```haskell
data GameState = GameState
  { ...
  , rng :: StdGen  -- Pure random generator
  , ...
  }

-- Function returns new generator along with result
randomPos :: Int -> Int -> StdGen -> (Position, StdGen)
```

This ensures:
- **Determinism**: Same seed → same game sequence
- **Replayability**: Recorded inputs can exactly recreate games
- **Testability**: Can unit test with known seeds

## Functional Patterns Used

### 1. Algebraic Data Types (ADTs)
```haskell
data Direction = U | D | L | R  -- Sum type

type Position = (Int, Int)  -- Product type

data Outcome = Menu | Running | Paused | GameOver  -- State machine as sum type
```

### 2. Higher-Order Functions
```haskell
-- Update bullets using map
updatedBullets = map (updateBullet dt) (bullets gs)

-- Filter valid bullets
validBullets = filter (\b -> inBounds w h (bulletPos b)) allBullets

-- Analytics using fold
avgScore = sum (map rFinalScore replays) `div` length replays
```

### 3. Function Composition
```haskell
-- Chaining transformations
renderPure :: GameState -> Picture
renderPure = pictures . map renderComponent . gameComponents
```

### 4. Pattern Matching
```haskell
advanceGameState gs =
  let newHead = case dir gs of
                  U -> (headX, headY + 1)
                  D -> (headX, headY - 1)
                  L -> (headX - 1, headY)
                  R -> (headX + 1, headY)
  in ...
```

### 5. Lazy Evaluation
```haskell
-- Replays are processed lazily
aggregateStats :: [Replay] -> String
-- Can handle large lists efficiently
```

## Replay System Architecture

### Recording
```haskell
data GameState = GameState
  { moveHistory :: [Direction]  -- Accumulated during gameplay
  , ...
  }

-- Each move appends to history
gs { moveHistory = currentDir : moveHistory gs }
```

### Persistence
```haskell
data Replay = Replay
  { rMoves :: [Direction]
  , rFinalScore :: Int
  , rDuration :: Float
  } deriving (Show, Read)  -- Automatic serialization

saveReplay :: Replay -> IO ()
```

### Analytics
```haskell
-- Pure aggregation function
aggregateStats :: [Replay] -> String
aggregateStats rs = 
  -- Uses higher-order functions
  -- map, filter, fold operations
  -- Produces summary statistics
```

## Advantages of This Architecture

### 1. **Testability**
- Pure functions can be tested without mocking
- No need to set up complex IO environments
- Can test any game state transformation in isolation

### 2. **Replayability**
- Deterministic behavior enables perfect replay
- Useful for debugging, analytics, and game balancing
- Can recreate any game scenario

### 3. **Maintainability**
- Clear separation of concerns
- Pure functions are easier to reason about
- Changes to rendering don't affect game logic

### 4. **Concurrency Safety**
- Immutable data structures are thread-safe
- No race conditions in game logic
- Could parallelize analytics without locks

### 5. **Composability**
- Small, pure functions compose into complex behavior
- Easy to add new features by composing existing functions
- Modular design supports incremental development

## Performance Considerations

### Immutability and Performance
- Haskell's lazy evaluation minimizes unnecessary computation
- Record updates use structural sharing (efficient copying)
- GHC optimizations handle most performance concerns

### Real-Time Requirements
- Game runs at 60 FPS with pure functional updates
- Time-based movement system ensures consistent gameplay
- Gloss library efficiently handles rendering

## Extension Points

The architecture makes it easy to extend:

1. **New Power-Ups**: Add variant to `PowerType` sum type
2. **New Game Modes**: Add variant to `Difficulty` or create new ADT
3. **AI Players**: Pure game state makes AI implementation straightforward
4. **Multiplayer**: Pure logic can be replicated across clients
5. **Advanced Analytics**: Add pure aggregation functions

## Conclusion

The Pure Core, Thin Shell architecture demonstrates that complex, real-time, stateful applications can be built with pure functional programming. The resulting code is clean, testable, and maintainable, proving that FP principles scale to interactive applications.
