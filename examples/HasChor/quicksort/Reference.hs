-- SPDX-FileCopyrightText: 2022 Gan Shen
-- SPDX-License-Identifier: BSD-3-Clause

-- Quicksort Reference Implementation

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = smaller ++ [x] ++ bigger
  where
    smaller = quicksort [a | a <- xs, a <= x]
    bigger = quicksort [a | a <- xs, a > x]

main :: IO ()
main = do
  x <- pure [6, 1, 3, 8, 4, 2, 5, 7, 9]
  print $ quicksort x
