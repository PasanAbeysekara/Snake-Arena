# Snake Arena

**A Pure-Functional Real-Time Snake Game Demonstrating Advanced FP Concepts**

Built entirely in Haskell using functional programming principles, Snake Arena proves that complex, stateful, real-time applications can be developed with pure functions, immutable data, and zero side effects in the core logic.

## Overview
Snake Arena is an interactive game where players control a snake on a 2D grid, collecting food and power-ups while avoiding obstacles, soldiers, and bullets. The game showcases functional programming excellence with:
- **100% pure game logic** - No mutations, no side effects
- **Deterministic behavior** - Same inputs always produce same results
- **Replay system** - Perfect game reproduction from minimal data
- **Real-time performance** - 60 FPS with immutable data structures
- **Type-safe domain modeling** - Invalid states are unrepresentable

Demo : https://www.youtube.com/watch?v=_yWrS3h9hDw

<img src="./docs/game-menu.png" alt="Game Menu" style="display: block; margin: 0 auto;">


<img src="./docs/gameplay.png" alt="Game Play" style="display: block; margin: 0 auto;">


## ✨ Functional Programming Features

### Pure Core, Thin Shell Architecture
- **Core Game Logic**: 100% pure functions in `Processing.hs` and `DataTypes.hs`
- **IO Boundary**: All side effects isolated in `Main.hs` and `IOHandler.hs`
- **Testability**: Pure functions are trivially testable without mocking

### Immutable Data Structures
- **No Mutations**: `GameState` is never modified, only transformed
- **Structural Sharing**: Efficient updates through Haskell's runtime optimization
- **Time Travel**: Previous states remain accessible for debugging

### Deterministic Randomness
- **Pure RNG**: `StdGen` threaded through state for reproducible randomness
- **Replay Capability**: Same seed + inputs = identical game every time
- **Testable Random Behavior**: Can unit test "random" scenarios

### Algebraic Data Types (ADTs)
- **Type Safety**: Compiler prevents invalid game states
- **Sum Types**: `Direction`, `Outcome`, `PowerType` model domain precisely  
- **Pattern Matching**: Exhaustive case analysis guaranteed by compiler

### Higher-Order Functions
- **Composable Logic**: `map`, `filter`, `fold` for entity updates
- **Declarative Style**: Express *what* not *how*
- **Analytics Pipeline**: Functional aggregation over replay data

### Lazy Evaluation
- **Efficient Processing**: Only compute what's needed
- **Infinite Structures**: Can work with potentially unbounded data
- **Performance**: Deferred evaluation optimizes chains of operations

## Game Features
- **Graphical UI**: Built with `gloss` library for functional graphics
- **Three Difficulty Levels**: Easy, Normal, Hard with dynamic parameters
- **Power-Ups**: 
  - 🔵 Speed Boost (Cyan) - Faster movement
  - 🟠 Score Multiplier (Orange) - 2x points
  - ❤️ Heart (Red) - Full health restoration
- **Dynamic Obstacles**: Tetris-like shapes spawn as you progress
- **Enemy Soldiers**: Spawn and shoot bullets at your snake
- **Health System**: Armor protects against bullet damage
- **Replay System**: Automatically saves game sessions to `.log` files
- **Analytics Dashboard**: View statistics from past games via console

## Installation
Prerequisites: `cabal`, `ghc`, and system libraries for `gloss` (OpenGL, GLUT).

```bash
# Update package lists
sudo apt-get update

# Ubuntu dependencies for gloss
sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev

# For headless servers (optional - only if no display available)
sudo apt-get install xvfb

# Build the project
cabal update
cabal build
```

## Usage

### Running on a Machine with Display
To run the game on a system with a graphical display:
```bash
cabal run snake-arena
```

### Running on a Headless Server
If you're on a server without a display (e.g., via SSH), use `xvfb-run`:
```bash
xvfb-run -a cabal run snake-arena
```

**Note**: On headless servers, the game will run but you won't be able to see or interact with it. For actual gameplay, run the game on a machine with a display, or set up VNC/X11 forwarding.

## 🎮 Controls
- **Menu**:
  - `1`: Easy Mode (Slow speed, 1x score, obstacles every 100pts)
  - `2`: Normal Mode (Medium speed, 2x score, obstacles every 70pts)
  - `3`: Hard Mode (Fast speed, 3x score, obstacles every 40pts)
  - `L`: Load Analytics (Display stats in console)
- **In-Game**:
  - `WASD` / `Arrow Keys`: Change snake direction
  - `P`: Pause game
- **Game Over**:
  - `R`: Return to main menu

## 🧠 Functional Programming Deep Dive

### The Power of Pure Functions

**Traditional Imperative Approach:**
```javascript
class Game {
  update() {
    this.snake[0].x += 1;        // Mutation!
    this.score += 10;            // Side effect!
    this.spawnEnemy();           // Unpredictable!
  }
}
```

**Snake Arena's Functional Approach:**
```haskell
step :: Float -> GameState -> GameState
step dt oldState = 
  let newState = oldState { timeSinceMove = timeSinceMove oldState + dt }
  in if timeSinceMove newState >= curSpeed newState
     then advanceGameState newState  -- Pure transformation
     else newState
```

### Why This Matters

1. **Referential Transparency**: Any function call can be replaced with its return value
   ```haskell
   let state1 = step 0.016 initialState
       state2 = step 0.016 initialState
   -- state1 == state2  ✓ Always true!
   ```

2. **Composability**: Complex behavior emerges from simple function composition
   ```haskell
   renderPure :: GameState -> Picture
   renderPure = pictures . map renderComponent . gameComponents
   ```

3. **Fearless Refactoring**: Type system catches breaking changes at compile time
   ```haskell
   data Direction = U | D | L | R  -- Add new direction?
   -- Compiler errors on every unhandled case!
   ```

### Key FP Patterns Used

#### 1. Immutable State Transformation
```haskell
data GameState = GameState
  { snake :: [Position]
  , score :: Int
  , rng :: StdGen  -- Even "randomness" is pure!
  , ...
  }

-- Old state → New state (old state unchanged)
gs' = gs { score = score gs + 10 }
```

#### 2. Algebraic Data Types
```haskell
-- Sum type: exactly one of these
data Outcome = Menu | Running | Paused | GameOver

-- Pattern matching ensures all cases handled
handleStatus Menu = renderMenu
handleStatus Running = renderGame
handleStatus Paused = renderPauseOverlay
handleStatus GameOver = renderGameOver
-- Compiler error if we forget a case!
```

#### 3. Pure Randomness with StdGen
```haskell
-- Deterministic "random" generation
randomPos :: Int -> Int -> StdGen -> (Position, StdGen)
randomPos w h gen =
  let (x, gen1) = randomR (-w `div` 2, w `div` 2) gen
      (y, gen2) = randomR (-h `div` 2, h `div` 2) gen1
  in ((x, y), gen2)  -- Returns new generator for next call
```

#### 4. Higher-Order Functions for Entity Management
```haskell
-- Update all soldiers without mutation
updatedSoldiers = map (updateSoldier dt) (soldiers gs)

-- Filter valid bullets declaratively
validBullets = filter isInBounds . filter notHitSnake $ bullets gs

-- Aggregate replay statistics
avgScore = sum (map rFinalScore replays) `div` length replays
```

#### 5. Function Composition
```haskell
-- Compose small functions into complex rendering
renderGame :: GameState -> Picture
renderGame gs = pictures
  [ renderBackground
  , renderObstacles gs
  , renderSnake gs
  , renderBullets gs
  , renderHUD gs
  ]
```

### The Replay System: FP's Killer Feature

Because the game is **pure and deterministic**, replays require only:
- Initial random seed
- Sequence of player inputs
- Timestamps

```haskell
data Replay = Replay
  { rMoves :: [Direction]      -- Just the inputs!
  , rFinalScore :: Int
  , rDuration :: Float
  } deriving (Show, Read)  -- Free serialization!
```

**Replay a game:**
```haskell
-- Same seed + same inputs = identical game
replayGame :: Replay -> StdGen -> GameState
replayGame replay seed = foldl applyMove initialState (rMoves replay)
```

**Benefits:**
- 📦 Tiny file sizes (KB instead of GB of video)
- 🎯 Perfect reproduction (not approximate)
- 🐛 Debug any game state by replaying to that point
- 📊 Analytics pipeline processes replays functionally
- 🧪 Test edge cases by replaying specific scenarios

## 📚 Documentation

- **[Architecture.md](Architecture.md)**: Deep dive into the Pure Core, Thin Shell pattern, module structure, and FP design decisions
- **[Challenges.md](Challenges.md)**: Technical challenges faced and functional solutions employed
- **[REPORT.md](REPORT.md)**: Academic project report with industry motivation

## 📁 Project Structure

```
src/
├── Main.hs           # IO boundary: rendering, input, game loop
├── DataTypes.hs      # Pure types: ADTs, GameState, all domain models
├── Processing.hs     # Pure core: game logic, collision, entity updates
├── IOHandler.hs      # IO effects: file save/load, persistence
└── Utils.hs          # Pure helpers: coordinates, analytics, utilities
```

**Separation of Concerns:**
- **Pure Modules** (`DataTypes`, `Processing`, `Utils`): Zero IO, 100% testable
- **IO Modules** (`Main`, `IOHandler`): Side effects isolated, thin layer

## 🔬 Why Functional Programming for Games?

### Traditional Concerns vs Reality

| Concern | Reality |
|---------|---------|
| "Immutability is slow" | Game runs at 60 FPS with structural sharing |
| "Can't have random without IO" | Pure RNG via StdGen threading works perfectly |
| "Need mutation for performance" | GHC optimizations handle immutable updates efficiently |
| "Too abstract for real-time apps" | Clear, maintainable code that's easier to reason about |

### Industry Applications

The patterns in Snake Arena apply to:
- **Financial Systems**: Deterministic transaction processing
- **Distributed Systems**: State replication without race conditions
- **Simulations**: Reproducible results from recorded inputs
- **Testing**: Pure functions enable property-based testing
- **Concurrency**: Immutable data structures are thread-safe by default

### Real Benefits Experienced

✅ **Zero null pointer exceptions** - Types prevent invalid states  
✅ **Fearless refactoring** - Compiler catches breaking changes  
✅ **Trivial testing** - No mocking, no setup, just pure functions  
✅ **Built-in time travel** - Previous states accessible for free  
✅ **Replay system** - Determinism enabled perfect game reproduction  
✅ **Easy debugging** - Can reproduce any bug from replay data  
✅ **Maintainable code** - Pure functions are self-documenting
