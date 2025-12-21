module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (newStdGen)
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import Control.Monad (forM)

import DataTypes
import Utils
import Processing
import IOHandler

main :: IO ()
main = do
  gen <- newStdGen
  savedHighScore <- loadHighScore
  -- Grid calculation: 
  -- Width: (1200 - 80) / 20 = 56 cells (40px walls on each side)
  -- Height: (800 - 140) / 20 = 33 cells (40px bottom + 100px top for HUD)
  let initState = (initialState 56 33 gen Normal) { status = Menu, hiScore = savedHighScore }
  -- Use playIO for side effects (saving replays, loading stats)
  playIO window background 60 initState renderIO handleInputIO updateIO

window :: Display
window = InWindow "🐍 Snake Arena" (windowWidth, windowHeight) (100, 100)

background :: Color
background = makeColorI 10 10 20 255  -- Dark blue-black

renderIO :: GameState -> IO Picture
renderIO gs = return $ renderPure gs

renderPure :: GameState -> Picture
renderPure gs = case status gs of
  Menu -> renderMenu gs
  Running -> renderGame gs
  Paused  -> pictures [renderGame gs, renderPauseOverlay]
  GameOver -> pictures [renderGame gs, renderGameOver gs]

-- Render Main Menu
renderMenu :: GameState -> Picture
renderMenu gs = pictures
  [ renderBackground
  , renderMenuBorder
  , renderTitle
  , renderMenuOptions
  , renderMenuFooter
  , renderHighScore gs
  ]
  where
    renderTitle = pictures
      [ color (makeColorI 100 50 200 100) $ translate (-320) 230 $ rectangleSolid 660 120
      , color (makeColorI 150 100 255 255) $ translate (-315) 235 $ rectangleSolid 650 110
      , color (makeColorI 255 255 100 255) $ translate (-280) 200 $ scale 0.8 0.8 $ text "SNAKE"
      , color (makeColorI 100 255 100 255) $ translate (-300) 120 $ scale 0.8 0.8 $ text "ARENA"
      ]
    
    renderMenuOptions = pictures
      [ renderButton 80 (makeColorI 50 200 50 255) "1" "EASY MODE" "Speed: Slow | Score: 1x | Obstacles: Every 100pts"
      , renderButton 0 (makeColorI 255 200 50 255) "2" "NORMAL MODE" "Speed: Medium | Score: 2x | Obstacles: Every 70pts"
      , renderButton (-80) (makeColorI 255 50 50 255) "3" "HARD MODE" "Speed: Fast | Score: 3x | Obstacles: Every 40pts"
      , color (makeColorI 100 200 255 255) $ translate (-200) (-180) $ scale 0.18 0.18 $ text "Press L: View Analytics"
      ]
    
    renderMenuFooter = pictures
      [ color (makeColorI 200 200 200 255) $ translate (-250) (-250) $ scale 0.15 0.15 $ text "Controls: WASD or Arrow Keys"
      , color (makeColorI 200 200 200 255) $ translate (-250) (-280) $ scale 0.15 0.15 $ text "Pause: P | Restart: R"
      ]

renderButton :: Float -> Color -> String -> String -> String -> Picture
renderButton yPos col key title desc = pictures
  [ color (makeColorI 30 30 40 200) $ translate (-250) yPos $ rectangleSolid 480 60
  , color col $ translate (-250) yPos $ rectangleWire 480 60
  , color col $ translate (-250) yPos $ thickRectangle 480 60 3
  , color col $ translate (-480) yPos $ circleSolid 12
  , color white $ translate (-470) (yPos - 5) $ scale 0.15 0.15 $ text key
  , color white $ translate (-420) (yPos + 5) $ scale 0.22 0.22 $ text title
  , color (makeColorI 180 180 180 255) $ translate (-420) (yPos - 15) $ scale 0.12 0.12 $ text desc
  ]

renderHighScore :: GameState -> Picture
renderHighScore gs = pictures
  [ color (makeColorI 255 215 0 255) $ translate 350 280 $ scale 0.2 0.2 $ text "HIGH SCORE"
  , color (makeColorI 255 255 255 255) $ translate 380 240 $ scale 0.35 0.35 $ text $ show (hiScore gs)
  ]

renderMenuBorder :: Picture
renderMenuBorder = pictures
  [ color (makeColorI 100 100 255 100) $ thickRectangle (fromIntegral windowWidth - 20) (fromIntegral windowHeight - 20) 4
  , color (makeColorI 150 150 255 60) $ thickRectangle (fromIntegral windowWidth - 40) (fromIntegral windowHeight - 40) 2
  ]

-- Render Game Screen
renderGame :: GameState -> Picture
renderGame gs = pictures
  [ renderBackground
  , renderBrickWalls
  , renderObstacles gs
  , renderSoldiers gs
  , renderBullets gs
  , renderSnake gs
  , renderFood gs
  , renderPowerUp gs
  , renderHUD gs
  , renderHealthBar gs
  ]

renderBackground :: Picture
renderBackground = pictures
  [ color (makeColorI 10 10 20 255) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
  , color (makeColorI 20 20 40 100) $ translate 200 150 $ circleSolid 300
  , color (makeColorI 30 20 50 80) $ translate (-250) (-100) $ circleSolid 400
  ]

renderGameBorder :: Picture
renderGameBorder = pictures
  [ color (makeColorI 50 150 255 255) $ thickRectangle (fromIntegral windowWidth - 100) (fromIntegral windowHeight - 220) 3
  , color (makeColorI 100 180 255 100) $ thickRectangle (fromIntegral windowWidth - 110) (fromIntegral windowHeight - 230) 2
  ]

-- Render brick wall border
renderBrickWalls :: Picture
renderBrickWalls = 
  let w = fromIntegral windowWidth
      h = fromIntegral windowHeight
      borderThickness = 40
      brickW = 30
      brickH = 15
  in pictures
    [ renderWallSide (-w/2 + borderThickness/2) 0 borderThickness h brickW brickH -- Left
    , renderWallSide (w/2 - borderThickness/2) 0 borderThickness h brickW brickH  -- Right
    , renderWallSide 0 (h/2 - 70 - borderThickness/2) w borderThickness brickW brickH -- Top
    , renderWallSide 0 (-h/2 + borderThickness/2) w borderThickness brickW brickH     -- Bottom
    ]

renderWallSide :: Float -> Float -> Float -> Float -> Float -> Float -> Picture
renderWallSide x y width height brickW brickH =
  let rows = ceiling (height / brickH) :: Int
      cols = ceiling (width / brickW) :: Int
      bricks = [ renderBrick (x + fromIntegral c * brickW - width/2) 
                            (y + fromIntegral r * brickH - height/2) 
                            brickW brickH (r `mod` 2 == 0)
               | r <- [0..rows-1], c <- [0..cols-1] ]
  in pictures bricks

renderBrick :: Float -> Float -> Float -> Float -> Bool -> Picture
renderBrick x y w h offset =
  let xOffset = if offset then w/2 else 0
      baseColor = makeColorI 120 60 30 255    -- Brown
      darkColor = makeColorI 80 40 20 255     -- Dark brown
      lightColor = makeColorI 140 70 35 255   -- Light brown
  in pictures
    [ color baseColor $ translate (x + xOffset) y $ rectangleSolid (w - 2) (h - 2)
    , color darkColor $ translate (x + xOffset) (y - h/2) $ rectangleSolid (w - 2) 1
    , color darkColor $ translate (x + xOffset - w/2) y $ rectangleSolid 1 (h - 2)
    , color lightColor $ translate (x + xOffset) (y + h/2) $ rectangleSolid (w - 2) 1
    , color lightColor $ translate (x + xOffset + w/2) y $ rectangleSolid 1 (h - 2)
    ]

-- Render obstacles
renderObstacles :: GameState -> Picture
renderObstacles gs = pictures [renderObstacleBrick pos | pos <- obstacles gs]
  where
    renderObstacleBrick pos =
      let (x, y) = gridToScreen pos
          size = fromIntegral cellPixelSize
          brickColor = makeColorI 100 50 25 255
          shadowColor = makeColorI 50 25 12 255
          highlightColor = makeColorI 150 75 37 255
      in pictures
        [ color shadowColor $ translate (x + 2) (y - 2) $ rectangleSolid size size
        , color brickColor $ translate x y $ rectangleSolid size size
        , color shadowColor $ translate x (y - size/2 + 2) $ rectangleSolid (size - 4) 4
        , color shadowColor $ translate (x - size/2 + 2) y $ rectangleSolid 4 (size - 4)
        , color highlightColor $ translate x (y + size/2 - 2) $ rectangleSolid (size - 4) 4
        , color highlightColor $ translate (x + size/2 - 2) y $ rectangleSolid 4 (size - 4)
        , color shadowColor $ translate x y $ rectangleWire size size
        ]

renderSnake :: GameState -> Picture
renderSnake gs = case snake gs of
  [] -> blank
  (h:t) -> pictures
    [ renderSnakeHead h (dir gs)
    , pictures [renderSnakeSegment i pos | (i, pos) <- zip [0..] t]
    ]
  where
    renderSnakeHead pos d = pictures
      [ color (makeColorI 100 255 100 255) $ uncurry translate (gridToScreen pos) $ circleSolid (fromIntegral cellPixelSize / 2 + 2)
      , color (makeColorI 50 200 50 255) $ uncurry translate (gridToScreen pos) $ circleSolid (fromIntegral cellPixelSize / 2)
      , renderEyes pos d
      ]
    
    renderEyes pos d = 
      let (x, y) = gridToScreen pos
          eyeOffset = 4
          (dx, dy) = case d of
            U -> (0, eyeOffset)
            D -> (0, -eyeOffset)
            L -> (-eyeOffset, 0)
            R -> (eyeOffset, 0)
          eye1 = translate (x + dx - 3) (y + dy + 2) $ color white $ circleSolid 2
          eye2 = translate (x + dx + 3) (y + dy + 2) $ color white $ circleSolid 2
      in pictures [eye1, eye2]
    
    renderSnakeSegment i pos =
      let intensity = 200 - (min 10 i * 10)
          (x, y) = gridToScreen pos
      in pictures
        [ color (makeColorI 50 (intensity + 20) 50 255) $ translate x y $ rectangleSolid (fromIntegral cellPixelSize - 1) (fromIntegral cellPixelSize - 1)
        , color (makeColorI 30 intensity 30 255) $ translate x y $ rectangleSolid (fromIntegral cellPixelSize - 4) (fromIntegral cellPixelSize - 4)
        ]

renderFood :: GameState -> Picture
renderFood gs = 
  let (x, y) = gridToScreen (foodPos gs)
  in pictures
    [ color (makeColorI 255 100 100 100) $ translate x y $ circleSolid (fromIntegral cellPixelSize / 2 + 4)
    , color (makeColorI 255 50 50 255) $ translate x y $ circleSolid (fromIntegral cellPixelSize / 2 + 2)
    , color (makeColorI 255 100 100 255) $ translate x y $ circleSolid (fromIntegral cellPixelSize / 2)
    ]

renderPowerUp :: GameState -> Picture
renderPowerUp gs = case powerPos gs of
  Nothing -> blank
  Just (pt, pos) -> 
    let (x, y) = gridToScreen pos
        pulse = 1.0 + 0.15 * sin (fromIntegral (score gs) * 0.3)
        (col1, col2, sym) = case pt of
          SpeedBoost -> (makeColorI 0 255 255 255, makeColorI 100 255 255 255, "⚡")
          ScoreMultiplier -> (makeColorI 255 150 0 255, makeColorI 255 200 100 255, "★")
          Heart -> (makeColorI 255 50 100 255, makeColorI 255 150 150 255, "♥")
    in case pt of
         Heart -> pictures
           [ color (makeColorI 255 100 150 150) $ translate x y $ circleSolid (14 * pulse)
           , color (makeColorI 255 50 100 255) $ translate x y $ renderHeart
           , color (makeColorI 255 200 200 255) $ translate x y $ thickCircle 8 1.5
           ]
         _ -> pictures
           [ color col2 $ translate x y $ rotate 45 $ rectangleSolid (fromIntegral cellPixelSize + 4) (fromIntegral cellPixelSize + 4)
           , color col1 $ translate x y $ rotate 45 $ rectangleSolid (fromIntegral cellPixelSize) (fromIntegral cellPixelSize)
           , color white $ translate (x - 8) (y - 8) $ scale 0.15 0.15 $ text sym
           ]

-- Render heart shape
renderHeart :: Picture
renderHeart = pictures
  [ translate (-4) 2 $ circleSolid 5
  , translate 4 2 $ circleSolid 5
  , color (makeColorI 255 50 100 255) $ polygon [(-8, 0), (0, -10), (8, 0), (0, 2)]
  ]

-- Render soldiers
renderSoldiers :: GameState -> Picture
renderSoldiers gs = pictures $ map renderSoldier (soldiers gs)

renderSoldier :: Soldier -> Picture
renderSoldier soldier =
  let (x, y) = gridToScreen (soldierPos soldier)
      bodyColor = makeColorI 150 50 50 255  -- Dark red body
      gunColor = makeColorI 80 80 80 255    -- Gray gun
      uniformColor = makeColorI 100 40 40 255
      
      -- Body (circle)
      body = color bodyColor $ translate x y $ circleSolid 9
      
      -- Uniform details
      uniform = color uniformColor $ translate x y $ circleSolid 7
      
      -- Gun pointing in direction
      gun = case soldierDir soldier of
              U -> color gunColor $ translate x (y + 8) $ rectangleSolid 4 10
              D -> color gunColor $ translate x (y - 8) $ rectangleSolid 4 10
              L -> color gunColor $ translate (x - 8) y $ rectangleSolid 10 4
              R -> color gunColor $ translate (x + 8) y $ rectangleSolid 10 4
      
      -- Helmet
      helmet = color (makeColorI 60 60 60 255) $ translate x (y + 3) $ rectangleSolid 6 4
      
  in pictures [body, uniform, gun, helmet]

-- Render bullets
renderBullets :: GameState -> Picture
renderBullets gs = pictures $ map renderBullet (bullets gs)

renderBullet :: Bullet -> Picture
renderBullet bullet =
  let (x, y) = gridToScreen (bulletPos bullet)
      bulletColor = makeColorI 255 255 0 255  -- Bright yellow
      glowColor = makeColorI 255 200 0 150    -- Orange glow
  in pictures
    [ color glowColor $ translate x y $ circleSolid 6
    , color bulletColor $ translate x y $ circleSolid 4
    , color white $ translate x y $ circleSolid 2
    ]

-- Render health bar
renderHealthBar :: GameState -> Picture
renderHealthBar gs =
  let maxHealth = 10
      currentHealth = health gs
      barWidth = 180
      barHeight = 18
      fillWidth = (fromIntegral currentHealth / fromIntegral maxHealth) * barWidth
      healthColor = if currentHealth > 7 then makeColorI 0 255 0 255
                   else if currentHealth > 4 then makeColorI 255 200 0 255
                   else makeColorI 255 0 0 255
      xPos = 0  -- Centered between HIGH SCORE and LENGTH
      yPos = fromIntegral windowHeight / 2 - 75  -- In the HUD panel
  in pictures
    [ color (makeColorI 40 40 60 255) $ translate xPos yPos $ rectangleSolid (barWidth + 4) (barHeight + 4)
    , color (makeColorI 60 60 60 255) $ translate xPos yPos $ rectangleSolid barWidth barHeight
    , color healthColor $ translate (xPos - (barWidth - fillWidth) / 2) yPos $ rectangleSolid fillWidth barHeight
    , color white $ translate (xPos - 100) (yPos - 20) $ scale 0.15 0.15 $ text "HEALTH"
    , color white $ translate (xPos + 60) (yPos - 20) $ scale 0.15 0.15 $ text $ show currentHealth ++ "/10"
    , color (makeColorI 255 255 255 150) $ translate xPos yPos $ rectangleWire barWidth barHeight
    ]
renderHUD :: GameState -> Picture
renderHUD gs = pictures
  [ renderScorePanel gs
  , renderPowerUpStatus gs
  ]
  where
    renderScorePanel gs = pictures
      [ color (makeColorI 20 20 40 220) $ translate 0 (fromIntegral windowHeight / 2 - 50) $ rectangleSolid (fromIntegral windowWidth) 100
      , color (makeColorI 100 150 255 255) $ translate 0 (fromIntegral windowHeight / 2 - 50) $ rectangleWire (fromIntegral windowWidth) 100
      , color (makeColorI 255 255 100 255) $ translate (-520) (fromIntegral windowHeight / 2 - 65) $ scale 0.25 0.25 $ text "SCORE"
      , color white $ translate (-520) (fromIntegral windowHeight / 2 - 95) $ scale 0.3 0.3 $ text $ show (score gs)
      , color (makeColorI 255 215 0 255) $ translate (-280) (fromIntegral windowHeight / 2 - 65) $ scale 0.2 0.2 $ text "HIGH SCORE"
      , color (makeColorI 255 255 255 255) $ translate (-280) (fromIntegral windowHeight / 2 - 95) $ scale 0.25 0.25 $ text $ show (hiScore gs)
      , color (makeColorI 100 255 255 255) $ translate 180 (fromIntegral windowHeight / 2 - 65) $ scale 0.2 0.2 $ text "LENGTH"
      , color white $ translate 180 (fromIntegral windowHeight / 2 - 95) $ scale 0.25 0.25 $ text $ show (length $ snake gs)
      , renderDifficultyIndicator gs
      ]
    
    renderDifficultyIndicator gs = 
      let (diffColor, diffText) = case difficulty gs of
                                    Easy -> (makeColorI 50 200 50 255, "EASY")
                                    Normal -> (makeColorI 255 200 50 255, "NORMAL")
                                    Hard -> (makeColorI 255 50 50 255, "HARD")
      in pictures
        [ color diffColor $ translate 420 (fromIntegral windowHeight / 2 - 65) $ scale 0.18 0.18 $ text diffText
        , color diffColor $ translate 420 (fromIntegral windowHeight / 2 - 85) $ scale 0.12 0.12 $ text $ case difficulty gs of
            Easy -> "x1 Score"
            Normal -> "x2 Score"
            Hard -> "x3 Score"
        ]
    
    renderPowerUpStatus gs = case activePower gs of
      Nothing -> blank
      Just pt -> 
        let (col, name) = case pt of
              SpeedBoost -> (makeColorI 0 255 255 255, "SPEED BOOST")
              ScoreMultiplier -> (makeColorI 255 150 0 255, "SCORE x2")
              Heart -> (makeColorI 255 50 100 255, "HEALTH RESTORED")  -- Heart doesn't activate but include for completeness
            barWidth = (powerTimer gs / 5.0) * 200
        in pictures
          [ color (makeColorI 20 20 40 220) $ translate 450 (fromIntegral windowHeight / 2 - 50) $ rectangleSolid 260 80
          , color col $ translate 450 (fromIntegral windowHeight / 2 - 50) $ rectangleWire 260 80
          , color col $ translate 450 (fromIntegral windowHeight / 2 - 35) $ scale 0.15 0.15 $ text name
          , color (makeColorI 60 60 80 255) $ translate 450 (fromIntegral windowHeight / 2 - 65) $ rectangleSolid 200 15
          , color col $ translate (450 - (200 - barWidth) / 2) (fromIntegral windowHeight / 2 - 65) $ rectangleSolid barWidth 15
          ]

-- Render Pause Overlay
renderPauseOverlay :: Picture
renderPauseOverlay = pictures
  [ color (makeColorI 0 0 0 180) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
  , color (makeColorI 255 255 100 255) $ translate (-120) 50 $ scale 0.6 0.6 $ text "PAUSED"
  , color white $ translate (-280) (-30) $ scale 0.25 0.25 $ text "Press P to Continue"
  , renderPauseBorder
  ]
  where
    renderPauseBorder = pictures
      [ color (makeColorI 255 255 100 255) $ thickRectangle 600 300 4
      , color (makeColorI 255 255 100 150) $ thickRectangle 620 320 2
      ]

-- Render Game Over Screen
renderGameOver :: GameState -> Picture
renderGameOver gs = pictures
  [ color (makeColorI 0 0 0 200) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
  , renderGameOverPanel gs
  ]
  where
    renderGameOverPanel gs = pictures
      [ color (makeColorI 40 20 20 240) $ rectangleSolid 700 450
      , color (makeColorI 255 50 50 255) $ thickRectangle 700 450 5
      , color (makeColorI 255 100 100 200) $ thickRectangle 680 430 2
      , color (makeColorI 255 50 50 255) $ translate (-220) 140 $ scale 0.7 0.7 $ text "GAME OVER"
      , renderStats gs
      , color (makeColorI 100 255 100 255) $ translate (-280) (-150) $ scale 0.22 0.22 $ text "Press R to Return to Menu"
      ]
    
    renderStats gs = pictures
      [ renderStatLine 50 "FINAL SCORE" (show $ score gs) (makeColorI 255 255 100 255)
      , renderStatLine (-20) "HIGH SCORE" (show $ hiScore gs) (makeColorI 255 215 0 255)
      , renderStatLine (-90) "SNAKE LENGTH" (show $ length $ snake gs) (makeColorI 100 255 255 255)
      ]
    
    renderStatLine y label value col = pictures
      [ color (makeColorI 200 200 200 255) $ translate (-250) y $ scale 0.25 0.25 $ text label
      , color col $ translate 50 y $ scale 0.35 0.35 $ text value
      ]

thickRectangle :: Float -> Float -> Float -> Picture
thickRectangle w h thickness = pictures
  [ translate 0 (h/2) $ rectangleSolid w thickness
  , translate 0 (-h/2) $ rectangleSolid w thickness
  , translate (w/2) 0 $ rectangleSolid thickness h
  , translate (-w/2) 0 $ rectangleSolid thickness h
  ]

-- | Update Loop with IO
updateIO :: Float -> GameState -> IO GameState
updateIO dt gs = do
  let newState = step dt gs
  -- Check for transition to GameOver
  if status gs == Running && status newState == GameOver
    then do
      -- Save Replay
      let moves = moveHistory gs -- get history
      -- We need to reconstruct duration, or just track it? 
      -- We didn't track total duration in GameState explicitly, only 'moveHistory'.
      -- But we can approximate or add 'totalTime' to GameState.
      -- For now, just use score.
      let replay = Replay
            { rMoves = reverse moves
            , rFinalScore = score gs
            , rDuration = 0.0 -- Todo: Add totalTime to GameState if needed
            }
      saveReplay replay
      -- Update and save high score if beaten
      let currentScore = score gs
      let currentHighScore = hiScore gs
      if currentScore > currentHighScore
        then do
          saveHighScore currentScore
          return newState { hiScore = currentScore }
        else return newState
    else return newState

-- | Input Loop with IO
handleInputIO :: Event -> GameState -> IO GameState
handleInputIO event gs = case (status gs, event) of
  (Menu, EventKey (Char '1') Down _ _) -> return $ startGame gs Easy
  (Menu, EventKey (Char '2') Down _ _) -> return $ startGame gs Normal
  (Menu, EventKey (Char '3') Down _ _) -> return $ startGame gs Hard
  
  (Menu, EventKey (Char 'l') Down _ _) -> do
    -- Load Analytics
    files <- listDirectory "."
    let logFiles = filter (".log" `isSuffixOf`) files
    replays <- mapM loadReplay logFiles
    let validReplays = [r | Right r <- replays]
    putStrLn "\n=== Analytics Report ==="
    putStrLn $ aggregateStats validReplays
    putStrLn "========================"
    return gs
    
  (Running, EventKey (SpecialKey KeyUp) Down _ _)    -> return $ Processing.handleInput U gs
  (Running, EventKey (SpecialKey KeyDown) Down _ _)  -> return $ Processing.handleInput D gs
  (Running, EventKey (SpecialKey KeyLeft) Down _ _)  -> return $ Processing.handleInput L gs
  (Running, EventKey (SpecialKey KeyRight) Down _ _) -> return $ Processing.handleInput R gs
  
  (Running, EventKey (Char 'w') Down _ _) -> return $ Processing.handleInput U gs
  (Running, EventKey (Char 's') Down _ _) -> return $ Processing.handleInput D gs
  (Running, EventKey (Char 'a') Down _ _) -> return $ Processing.handleInput L gs
  (Running, EventKey (Char 'd') Down _ _) -> return $ Processing.handleInput R gs
  
  (Running, EventKey (Char 'p') Down _ _) -> return $ gs { status = Paused }
  (Paused, EventKey (Char 'p') Down _ _)  -> return $ gs { status = Running }
  
  (GameOver, EventKey (Char 'r') Down _ _) -> return $ gs { status = Menu }
  
  _ -> return gs

startGame :: GameState -> Difficulty -> GameState
startGame oldGs diff =
  let w = 56  -- Match the grid size
      h = 33  -- Adjusted for proper wall alignment
      g = rng oldGs
      savedHiScore = hiScore oldGs  -- Preserve high score
      newState = initialState w h g diff
  in newState { hiScore = savedHiScore }
