module IOHandler where

import DataTypes
import System.IO
import Control.Exception (try, IOException)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Save a replay to a file
saveReplay :: Replay -> IO ()
saveReplay replay = do
  timestamp <- getCurrentTime
  let filename = "replay_" ++ formatTime defaultTimeLocale "%Y%m%d_%H%M%S" timestamp ++ ".log"
  writeFile filename (show replay)
  putStrLn $ "Replay saved to " ++ filename

-- | Load a replay from a file
loadReplay :: FilePath -> IO (Either String Replay)
loadReplay path = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left err -> return $ Left (show err)
    Right content -> return $ case reads content of
      [(r, "")] -> Right r
      _         -> Left "Parse error"

-- | Initialize or Check storage (Optional)
initStorage :: IO ()
initStorage = return ()

-- | Save high score to file
saveHighScore :: Int -> IO ()
saveHighScore score = writeFile "highscore.dat" (show score)

-- | Load high score from file, return 0 if file doesn't exist
loadHighScore :: IO Int
loadHighScore = do
  result <- try (readFile "highscore.dat") :: IO (Either IOException String)
  case result of
    Left _ -> return 0
    Right content -> case reads content of
      [(s, "")] -> return s
      _         -> return 0
