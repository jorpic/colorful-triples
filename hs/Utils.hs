module Utils where

import Text.Printf (printf)
import System.CPUTime (getCPUTime)

timeIt :: IO a -> IO a
timeIt f = do
  s <- getCPUTime
  r <- f
  e <- getCPUTime
  let delta = e - s `div` 10^(9::Int) :: Integer
  putStrLn $ "time: " ++ show delta
  return r

info :: Show val => String -> val -> IO ()
info msg val = printf msg (show val) >> putStrLn ""
