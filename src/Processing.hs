module Processing where

import DataTypes
import Utils
import System.Random (StdGen, randomR, split)

-- | Initial game state
initialState :: Int -> Int -> StdGen -> Difficulty -> GameState
initialState w h gen diff =
  let (startPos, gen1) = randomPos w h gen
      (food, gen2) = randomPos w h gen1
      baseSpeed = case diff of
                    Easy -> 0.20
                    Normal -> 0.15
                    Hard -> 0.08
  in GameState
     { snake = [startPos, (fst startPos, snd startPos - 1)]
     , dir = U
     , nextDir = U
     , foodPos = food
     , powerPos = Nothing
     , score = 0
     , hiScore = 0 -- Should be loaded, but simple for now
     , gridWidth = w
     , gridHeight = h
     , status = Running
     , rng = gen2
     , timeSinceMove = 0.0
     , curSpeed = baseSpeed
     , powerTimer = 0.0
     , activePower = Nothing
     , moveHistory = []
     , obstacles = []
     , difficulty = diff
     }

-- | Handle input events to change direction
handleInput :: Direction -> GameState -> GameState
handleInput newDir gs
  | status gs /= Running = gs
  | isOpposite (dir gs) newDir = gs
  | otherwise = gs { nextDir = newDir }
  where
    isOpposite U D = True
    isOpposite D U = True
    isOpposite L R = True
    isOpposite R L = True
    isOpposite _ _ = False

-- | Main update loop
step :: Float -> GameState -> GameState
step dt gs
  | status gs /= Running = gs
  | otherwise =
      let newTime = timeSinceMove gs + dt
          newPowerTimer = max 0 (powerTimer gs - dt)
          active = if newPowerTimer == 0 then Nothing else activePower gs
          -- Reset speed if powerup expires
          baseSpeed = case difficulty gs of
                        Easy -> 0.20
                        Normal -> 0.15
                        Hard -> 0.08
          speed = case active of
                    Just SpeedBoost -> case difficulty gs of
                                         Easy -> 0.10
                                         Normal -> 0.08
                                         Hard -> 0.05
                    _ -> baseSpeed
      in if newTime >= speed
         then advanceGameState (gs { timeSinceMove = 0, powerTimer = newPowerTimer, activePower = active })
         else gs { timeSinceMove = newTime, powerTimer = newPowerTimer, activePower = active }

-- | Advance game state by one tick
advanceGameState :: GameState -> GameState
advanceGameState gs =
  let currentDir = nextDir gs
      (headX, headY) = head (snake gs)
      newHead = case currentDir of
                  U -> (headX, headY + 1)
                  D -> (headX, headY - 1)
                  L -> (headX - 1, headY)
                  R -> (headX + 1, headY)
      
      -- Check collisions
      hitWall = not (inBounds (gridWidth gs) (gridHeight gs) newHead)
      hitSelf = newHead `elem` init (snake gs) -- tail allows following
      hitObstacle = newHead `elem` obstacles gs  -- Check obstacle collision

      -- Food
      ateFood = newHead == foodPos gs
      
      -- PowerUp
      atePower = case powerPos gs of
                   Just (pt, pos) -> pos == newHead
                   Nothing -> False
      
      -- New Snake
      newSnake = newHead : if ateFood then snake gs else init (snake gs)
  
  in if hitWall || hitSelf || hitObstacle
     then gs { status = GameOver }
     else let
            (newRng, newFood, newPowerPos) = spawnItems ateFood atePower gs
            
            -- Difficulty multiplier for base score
            diffMult = case difficulty gs of
                         Easy -> 1
                         Normal -> 2
                         Hard -> 3
            -- PowerUp multiplier
            powerMult = if activePower gs == Just ScoreMultiplier then 2 else 1
            newScore = score gs + (if ateFood then 10 * diffMult * powerMult else 0)
            
            newActivePower = if atePower
                             then case powerPos gs of
                                    Just (pt, _) -> Just pt
                                    Nothing -> activePower gs
                             else activePower gs
            
            -- Difficulty-based power-up duration
            powerDuration = case difficulty gs of
                              Easy -> 12.0
                              Normal -> 10.0
                              Hard -> 8.0
            newPowerTimer = if atePower then powerDuration else powerTimer gs
            
            -- Generate obstacles based on score milestones
            newObstacles = generateObstacles gs newScore newRng

          in gs
             { snake = newSnake
             , dir = currentDir
             , score = newScore
             , foodPos = newFood
             , powerPos = newPowerPos
             , rng = newRng
             , activePower = newActivePower
             , powerTimer = newPowerTimer
             , moveHistory = currentDir : moveHistory gs
             , obstacles = newObstacles
             }

-- | Spawn food and powerups
spawnItems :: Bool -> Bool -> GameState -> (StdGen, Position, Maybe (PowerType, Position))
spawnItems ateFood atePower gs =
  let gen0 = rng gs
      
      -- Spawn Food
      (food, gen1) = if ateFood
                     then randomPos (gridWidth gs) (gridHeight gs) gen0
                     else (foodPos gs, gen0) -- Keep existing
      
      -- Spawn PowerUp? (Random chance if not exists)
      shouldSpawnPower = case powerPos gs of
                           Just _ -> False -- Already one
                           Nothing -> fst (randomR (1 :: Int, 100) gen1) > 95 -- 5% chance per tick if empty? No, per move.
      
      (ptIdx, gen2) = randomR (0 :: Int, 1) gen1
      pt = if ptIdx == 0 then SpeedBoost else ScoreMultiplier
      (ppos, gen3) = randomPos (gridWidth gs) (gridHeight gs) gen2
      
      newPower = if atePower
                 then Nothing -- Consumed
                 else case powerPos gs of
                        Just existing -> Just existing
                        Nothing -> if shouldSpawnPower then Just (pt, ppos) else Nothing

  in (gen3, food, newPower)

-- | Obstacle shape patterns (relative positions from origin)
obstacleShapes :: [[Position]]
obstacleShapes = 
  [ -- L shape
    [(0,0), (0,1), (0,2), (1,0)]
  , -- Reverse L
    [(0,0), (0,1), (0,2), (-1,0)]
  , -- T shape
    [(0,0), (-1,0), (1,0), (0,1)]
  , -- Z shape
    [(0,0), (1,0), (0,1), (-1,1)]
  , -- S shape
    [(0,0), (-1,0), (0,1), (1,1)]
  , -- I shape (horizontal)
    [(0,0), (1,0), (2,0), (3,0)]
  , -- I shape (vertical)
    [(0,0), (0,1), (0,2), (0,3)]
  , -- Small square
    [(0,0), (1,0), (0,1), (1,1)]
  , -- Plus shape
    [(0,0), (-1,0), (1,0), (0,1), (0,-1)]
  , -- Corner
    [(0,0), (1,0), (2,0), (0,1), (0,2)]
  ]

-- | Generate obstacles based on score milestones (difficulty-based intervals)
generateObstacles :: GameState -> Int -> StdGen -> [Position]
generateObstacles gs newScore gen =
  let currentShapeCount = length (obstacles gs) `div` 4  -- Approximate shape count
      -- Difficulty-based obstacle parameters
      (scoreInterval, maxShapes) = case difficulty gs of
                                     Easy -> (100, 4)   -- 1 shape every 100 points, max 4
                                     Normal -> (70, 6)  -- 1 shape every 70 points, max 6
                                     Hard -> (40, 10)   -- 1 shape every 40 points, max 10
      -- Calculate how many shapes should exist based on score
      targetShapeCount = min maxShapes (newScore `div` scoreInterval)
      
  in if targetShapeCount > currentShapeCount
     then 
       -- Add new obstacle shapes
       let toAdd = targetShapeCount - currentShapeCount
           newObs = generateSafeObstacleShapes toAdd gs gen
       in obstacles gs ++ newObs
     else obstacles gs

-- | Generate obstacle shapes that don't overlap with snake, food, or powerups
generateSafeObstacleShapes :: Int -> GameState -> StdGen -> [Position]
generateSafeObstacleShapes 0 _ _ = []
generateSafeObstacleShapes n gs gen =
  let -- Pick random shape
      (shapeIdx, gen1) = randomR (0, length obstacleShapes - 1) gen
      shapePattern = obstacleShapes !! shapeIdx
      
      -- Pick random origin position
      (originPos, gen2) = randomPos (gridWidth gs) (gridHeight gs) gen1
      
      -- Calculate actual positions for this shape
      shapePositions = map (\(dx, dy) -> (fst originPos + dx, snd originPos + dy)) shapePattern
      
      -- Check if all positions are valid
      occupied = snake gs ++ [foodPos gs] ++ 
                 (case powerPos gs of
                    Just (_, p) -> [p]
                    Nothing -> []) ++
                 obstacles gs
      
      -- Also avoid center area where snake starts
      centerZone = [(x, y) | x <- [-5..5], y <- [-5..5]]
      forbidden = occupied ++ centerZone
      
      -- Check if shape fits within bounds and doesn't overlap
      allInBounds = all (\pos -> inBounds (gridWidth gs) (gridHeight gs) pos) shapePositions
      noOverlap = not (any (`elem` forbidden) shapePositions)
      
  in if allInBounds && noOverlap
     then shapePositions ++ generateSafeObstacleShapes (n - 1) gs gen2
     else generateSafeObstacleShapes n gs gen2  -- Try again with new position
