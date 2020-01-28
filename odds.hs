module Odds where

import Data.List

-- K items chosen from a list of n items
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)