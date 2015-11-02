listSize :: [a] -> Int
listSize [] = 0
listSize (_:xs) = 1 + listSize xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert e (x:xs)
            | e <= x = e : x : xs
            | e > x = x : insert e xs

zip2lists :: [a] -> [b] -> [(a,b)]
zip2lists _ [] = []
zip2lists [] _ = []
zip2lists (x:xs) (y:ys) = (x,y) : zip2lists xs ys