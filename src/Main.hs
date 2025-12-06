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
  let initState = (initialState 40 30 gen) { status = Menu }
  -- Use playIO for side effects (saving replays, loading stats)
  playIO window background 60 initState renderIO handleInputIO updateIO

window :: Display
window = InWindow "Snake Arena" (windowWidth, windowHeight) (100, 100)

background :: Color
background = black

renderIO :: GameState -> IO Picture
renderIO gs = return $ renderPure gs

renderPure :: GameState -> Picture
renderPure gs = case status gs of
  Menu -> pictures
    [ color white $ translate (-150) 50 $ text "SNAKE ARENA"
    , color yellow $ translate (-200) 0 $ scale 0.2 0.2 $ text "Press 1: Easy | 2: Normal | 3: Hard"
    , color green $ translate (-200) (-30) $ scale 0.2 0.2 $ text "Press L: Load Analytics (console)"
    , color white $ translate (-200) (-60) $ scale 0.2 0.2 $ text "WASD/Arrows to Move | P to Pause"
    ]
  Running -> renderGame gs
  Paused  -> pictures [renderGame gs, color yellow $ translate (-100) 0 $ text "PAUSED"]
  GameOver -> pictures
    [ renderGame gs
    , color red $ translate (-200) 0 $ text "GAME OVER"
    , color white $ translate (-200) (-50) $ scale 0.3 0.3 $ text $ "Final Score: " ++ show (score gs)
    , color white $ translate (-250) (-100) $ scale 0.2 0.2 $ text "Press R to Restart"
    ]

renderGame :: GameState -> Picture
renderGame gs = pictures
  [ color blue $ rectangleWire (fromIntegral windowWidth - 10) (fromIntegral windowHeight - 10)
  , pictures [ color green $ uncurry translate (gridToScreen pos) $ rectangleSolid (fromIntegral cellPixelSize - 2) (fromIntegral cellPixelSize - 2) | pos <- snake gs ]
  , color red $ uncurry translate (gridToScreen (foodPos gs)) $ circleSolid (fromIntegral cellPixelSize / 2)
  , case powerPos gs of
      Just (pt, pos) ->
        let c = case pt of SpeedBoost -> cyan; ScoreMultiplier -> orange
        in color c $ uncurry translate (gridToScreen pos) $ rectangleSolid (fromIntegral cellPixelSize - 4) (fromIntegral cellPixelSize - 4)
      Nothing -> blank
  , color white $ translate (fromIntegral (-windowWidth `div` 2) + 20) (fromIntegral (windowHeight `div` 2) - 40) $ scale 0.2 0.2 $ text $ "Score: " ++ show (score gs)
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
      return newState
    else return newState

-- | Input Loop with IO
handleInputIO :: Event -> GameState -> IO GameState
handleInputIO event gs = case (status gs, event) of
  (Menu, EventKey (Char '1') Down _ _) -> return $ startGame gs 0.20
  (Menu, EventKey (Char '2') Down _ _) -> return $ startGame gs 0.15
  (Menu, EventKey (Char '3') Down _ _) -> return $ startGame gs 0.08
  
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

startGame :: GameState -> Float -> GameState
startGame oldGs spd =
  let w = gridWidth oldGs
      h = gridHeight oldGs
      g = rng oldGs
      newState = initialState w h g
  in newState { curSpeed = spd }
