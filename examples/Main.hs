module Main (main) where

import Functions qualified
import HKD1 qualified
import HKD2 qualified

main :: IO ()
main = do
  Functions.test
  HKD1.test
  HKD2.test
