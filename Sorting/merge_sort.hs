merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) 
    else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

m_sort :: Ord a => [a] -> [a]
m_sort [] = []
m_sort [x] = [x]
m_sort xs = merge (m_sort l) (m_sort r)
        where (l, r) = halve xs
            