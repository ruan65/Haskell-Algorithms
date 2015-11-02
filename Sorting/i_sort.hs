insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert e (x:xs)
            | e <= x = e : x : xs
            | e > x = x : insert e xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)