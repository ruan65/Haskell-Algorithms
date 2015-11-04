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

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (_:xs) = dropN (n - 1) xs

inity :: [a] -> [a]
inity [_] = []
inity (x:xs) = x : inity xs

evn :: Int -> Bool
evn 0 = True
evn n = odn (n-1)

odn :: Int -> Bool
odn 0 = False
odn n = evn (n-1)

pow :: Int -> Int -> Int
pow m 0 = 1
pow m n = m * m * pow m (n-2)

andy :: [Bool] -> Bool
andy [] = True
andy (b:bs) | b = andy bs
            | otherwise = False

conc :: [[a]] -> [a]
conc [[]] = []
conc (xs:xss) = xs ++ conc xss


