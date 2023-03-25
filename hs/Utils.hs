module Utils where

import Text.Printf (printf)
import System.CPUTime (getCPUTime)

timeIt :: IO a -> IO a
timeIt f = do
  s <- getCPUTime
  r <- f
  e <- getCPUTime
  putStrLn $ "time: " ++ show ((fromIntegral $ e - s) / (10^9))
  return r

info :: Show val => String -> val -> IO ()
info msg val = printf msg (show val) >> putStrLn ""
