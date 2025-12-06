# Project Report: Snake Arena

## 1. Introduction
Snake Arena is a graphical implementation of the classic Snake game, designed to demonstrate functional programming principles in a state-heavy real-time application. The goal was to build a playable game using a "pure core, thin shell" architecture, separating the pure game logic from the side-effects of rendering and user input.

## 2. Industry Motivation
Real-world systems, such as financial exchanges or reliable simulation engines, benefit from deterministic state transitions. By modeling the application state as an immutable record and transformations as pure functions, we gain:
- **Testability**: Any state can be unit-tested without complex setup.
- **Replayability**: Since the logic is deterministic, replaying a sequence of inputs guarantees the exact same outcome.
- **Concurrency Safety**: Immutable data structures can be shared across threads without locking.

## 3. Functional Design

### 3.1 State Management
The `GameState` is modeled as a Haskell Record, containing all necessary information (snake body, food position, score, random seed, etc.). `GameState` is immutable.

```haskell
data GameState = GameState { snake :: [Position], dir :: Direction, ... }
```

### 3.2 The Pure Update Step
The core engine is defined in `Processing.hs`. The `step` function takes a time delta and the current state, returning a new state.

```haskell
step :: Float -> GameState -> GameState
```

This function handles:
1. **Time Accumulation**: Waiting for the next movement tick.
2. **Collision Detection**: Checking walls and self-intersection.
3. **State Transition**: Moving the snake, growing, or spawning items.

The `System.Random.StdGen` is passed through the state, ensuring that even randomness is functionally pure and deterministic given the initial seed.

### 3.3 IO & Replays
Side effects are confined to `Main.hs` (gloss loop) and `IOHandler.hs`.
- **Replays**: At the end of a game, the sequence of moves and final score is serialized to a log file.
- **Analytics**: A functional pipeline reads these files, parses them (`String -> Replay`), and folds over them to compute aggregates (Average Score, Max Score).

## 4. Key FP Concepts Used

1. **Algebraic Data Types (ADTs)**:
   Used `Direction` (Sum Type), `Position` (Product Type), and `Outcome` to make invalid states unrepresentable.

2. **Higher-Order Functions**:
   `map`, `filter`, and custom update logic rely on passing functions. The Gloss `playIO` function itself is a higher-order wrapper around the game loop.

3. **Immutability**:
   No `IORef` or `MVar` was used for game logic. Every frame produces a fresh `GameState`.

4. **Lazy Evaluation**:
   Used in list processing for analytics, allowing efficient processing of large lists of replays (potentially).

## 5. Conclusion
Snake Arena successfully demonstrates that interactive, real-time applications can be built with strict functional purity. The resulting code is modular, easy to reason about, and supports advanced features like deterministic replays with minimal additional effort.
