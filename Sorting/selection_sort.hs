selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minVal : (selectionSort deletedMin)
                   where deletedMin = [x | x <- xs, x /= minVal]
                         minVal = minimum xs