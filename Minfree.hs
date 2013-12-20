module Minfree
       where

import Data.List

-- version 0: O(n^2)
--minfree xs = head ([0..] \\ xs)

-- version final
minfree xs = minfrom 0 (length xs, xs)
minfrom a (n, xs) 
             | n == 0     = a
             | m == b-a   = minfrom b (n-m, vs)
             | otherwise  = minfrom a (m, us)
               where (us, vs) = partition (< b) xs
                     b        = a + 1 + n `div` 2 
                     m        = length us