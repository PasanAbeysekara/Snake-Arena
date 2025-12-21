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
     , health = 10
     , soldiers = []
     , bullets = []
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
          
          -- Update soldiers (decrease shoot timers, reset when reaches 0)
          updatedSoldiers = map (updateSoldier dt) (soldiers gs)
          -- Generate new bullets from soldiers ready to shoot
          newlyFiredBullets = concatMap (\s -> if shootTimer s == 0 then [Bullet (soldierPos s) (soldierDir s) 0.0] else []) updatedSoldiers
          -- Reset shoot timers for soldiers that just fired
          soldiersFinal = map (\s -> if shootTimer s == 0 then s { shootTimer = 2.0 } else s) updatedSoldiers
          
          -- Update existing bullets position
          updatedBullets = updateBullets dt gs
          allBullets = updatedBullets ++ newlyFiredBullets
          
          -- Check bullet collisions with snake
          snakePositions = snake gs
          bulletHits = filter (\b -> bulletPos b `elem` snakePositions) allBullets
          numHits = length bulletHits
          newHealth = max 0 (health gs - numHits)
          
          -- Remove bullets that hit snake or went out of bounds
          validBullets = filter (\b -> inBounds (gridWidth gs) (gridHeight gs) (bulletPos b) 
                                       && bulletPos b `notElem` snakePositions
                                       && bulletPos b `notElem` obstacles gs) allBullets
          
          gs' = gs { soldiers = soldiersFinal
                   , bullets = validBullets
                   , health = newHealth
                   , timeSinceMove = newTime
                   , powerTimer = newPowerTimer
                   , activePower = active
                   }
      in if newHealth <= 0
         then gs' { status = GameOver }
         else if newTime >= speed
              then advanceGameState (gs' { timeSinceMove = 0 })
              else gs'

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
      
      -- Check if ate soldier
      ateSoldier = any (\s -> soldierPos s == newHead) (soldiers gs)
      newSoldiersList = filter (\s -> soldierPos s /= newHead) (soldiers gs)

      -- Food
      ateFood = newHead == foodPos gs
      
      -- PowerUp
      atePower = case powerPos gs of
                   Just (pt, pos) -> pos == newHead
                   Nothing -> False
      
      -- New Snake
      newSnake = newHead : if ateFood || ateSoldier then snake gs else init (snake gs)
  
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
            -- Soldier bonus
            soldierBonus = if ateSoldier then 50 * diffMult else 0
            newScore = score gs + (if ateFood then 10 * diffMult * powerMult else 0) + soldierBonus
            
            newActivePower = if atePower
                             then case powerPos gs of
                                    Just (Heart, _) -> activePower gs  -- Heart doesn't activate as power
                                    Just (pt, _) -> Just pt
                                    Nothing -> activePower gs
                             else activePower gs
            
            -- Difficulty-based power-up duration
            powerDuration = case difficulty gs of
                              Easy -> 12.0
                              Normal -> 10.0
                              Hard -> 8.0
            newPowerTimer = if atePower
                            then case powerPos gs of
                                   Just (Heart, _) -> powerTimer gs  -- Heart doesn't have timer
                                   _ -> powerDuration
                            else powerTimer gs
            
            -- Heal if ate heart
            newHealth = if atePower
                        then case powerPos gs of
                               Just (Heart, _) -> 10  -- Full health restore
                               _ -> health gs
                        else health gs
            
            -- Generate obstacles based on score milestones
            newObstacles = generateObstacles gs newScore newRng
            
            -- Spawn soldiers based on score
            (newSoldiersSpawned, newRng2) = spawnSoldiers gs newScore newRng

          in gs
             { snake = newSnake
             , dir = currentDir
             , score = newScore
             , foodPos = newFood
             , powerPos = newPowerPos
             , rng = newRng2
             , activePower = newActivePower
             , powerTimer = newPowerTimer
             , moveHistory = currentDir : moveHistory gs
             , obstacles = newObstacles
             , soldiers = newSoldiersList ++ newSoldiersSpawned
             , health = newHealth
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
                           Nothing -> fst (randomR (1 :: Int, 100) gen1) > 93 -- 7% chance
      
      (ptIdx, gen2) = randomR (0 :: Int, 2) gen1  -- Now 3 types: 0=SpeedBoost, 1=ScoreMultiplier, 2=Heart
      pt = case ptIdx of
             0 -> SpeedBoost
             1 -> ScoreMultiplier
             2 -> Heart
             _ -> SpeedBoost
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

-- | Update soldier shoot timer
updateSoldier :: Float -> Soldier -> Soldier
updateSoldier dt soldier = soldier { shootTimer = max 0 (shootTimer soldier - dt) }

-- | Update bullet positions - bullets move fast
updateBullets :: Float -> GameState -> [Bullet]
updateBullets dt gs =
  let moveBullet b =
        let newSpeed = bulletSpeed b + dt * 10.0  -- Bullets move 10 cells per second
            steps = floor newSpeed :: Int
            remaining = newSpeed - fromIntegral steps
            newPos = moveBulletSteps (bulletPos b) (bulletDir b) steps
        in b { bulletPos = newPos, bulletSpeed = remaining }
  in map moveBullet (bullets gs)

-- | Move bullet multiple steps in its direction
moveBulletSteps :: Position -> Direction -> Int -> Position
moveBulletSteps pos _ 0 = pos
moveBulletSteps (x, y) dir n = moveBulletSteps newPos dir (n - 1)
  where newPos = case dir of
                   U -> (x, y + 1)
                   D -> (x, y - 1)
                   L -> (x - 1, y)
                   R -> (x + 1, y)

-- | Spawn soldiers at score milestones
spawnSoldiers :: GameState -> Int -> StdGen -> ([Soldier], StdGen)
spawnSoldiers gs newScore gen =
  let currentCount = length (soldiers gs)
      (scoreInterval, maxSoldiers) = case difficulty gs of
                                       Easy -> (150, 3)    -- Easy: soldiers every 150 pts, max 3
                                       Normal -> (100, 5)  -- Normal: every 100 pts, max 5
                                       Hard -> (80, 8)     -- Hard: every 80 pts, max 8
      targetCount = min maxSoldiers (newScore `div` scoreInterval)
  in if targetCount > currentCount
     then let (soldier, gen') = generateSafeSoldier gs gen
          in ([soldier], gen')
     else ([], gen)

-- | Generate a soldier in a safe position
generateSafeSoldier :: GameState -> StdGen -> (Soldier, StdGen)
generateSafeSoldier gs gen =
  let w = gridWidth gs
      h = gridHeight gs
      (x, gen1) = randomR (-w `div` 2 + 3, w `div` 2 - 3) gen
      (y, gen2) = randomR (-h `div` 2 + 3, h `div` 2 - 3) gen1
      (dirIdx, gen3) = randomR (0 :: Int, 3) gen2
      dir = [U, D, L, R] !! dirIdx
      pos = (x, y)
      
      -- Check if position is safe
      occupied = snake gs ++ [foodPos gs] ++ obstacles gs ++
                 map soldierPos (soldiers gs) ++
                 (case powerPos gs of
                    Just (_, p) -> [p]
                    Nothing -> [])
  in if pos `elem` occupied
     then generateSafeSoldier gs gen3  -- Try again
     else (Soldier pos dir 2.0, gen3)  -- Shoot every 2 seconds
