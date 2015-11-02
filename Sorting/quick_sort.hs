quickSort :: [Int] -> [Int]

quickSort [] = []

quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
        where left = [n | n <- xs, n <= x]
              right = [k | k <- xs, k > x]
