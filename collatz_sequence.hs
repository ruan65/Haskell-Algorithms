collatz :: Int -> [Int]
collatz n | n == 1 = [1]
          | even n = n : collatz (n `div` 2)
          | odd n = n : collatz (3*n + 1) 