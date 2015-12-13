

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

--filter
separate :: (a -> Bool) -> [a] -> [a]
separate f [] = []
separate f (x:xs) | f x = x: separate f xs
                  | otherwise = separate f xs

filteri :: (a -> Bool) -> [a] -> [a]
filteri f xs = [x | x <- xs, f x]

-- all
allAre :: (a -> Bool) -> [a] -> Bool
allAre f xs = and [f x | x <- xs]

-- any
anyAre :: (a -> Bool) -> [a] -> Bool
anyAre f xs = or [f x | x <- xs]

-- foldr
foldri :: (a -> b -> b) -> b -> [a] -> b
foldri f v [] = v
foldri f v (x:xs) = f x (foldri f v xs)

snoc :: a -> [a] -> [a]
snoc a xs = xs ++ [a]

-- reverse
reversi :: [a] -> [a]
reversi = foldri snoc []

-- .
point :: (b -> c) -> (a -> b) -> (a -> c)
point f g = \x -> f (g x)

dot :: (a -> b) -> (c -> a) -> (c -> b)
dot f g x = f (g x)


repli :: a -> Int -> [a]
repli x n = case n of 0 -> []
                      n -> x : repli x (n-1)

taki :: Int -> [a] -> [a]
taki n (x:xs) | n == 0 = []
              | otherwise = x : taki (n-1) xs

elemi :: (Eq a) => a -> [a] -> Bool
elemi _ [] = False
elemi el (x:xs) | el == x = True
                | otherwise = elemi el xs

applyThenZip :: (a -> b -> c) -> [a] -> [b] -> [c]
applyThenZip f _ [] = []
applyThenZip f [] _ = []
applyThenZip f (x:xs) (y:ys) = f x y : applyThenZip f xs ys

mappir :: (a -> b) -> [a] -> [b]
mappir f = foldr (\x xs -> f x : xs) []

mappil :: (a -> b) -> [a] -> [b]
mappil f = foldl (\xs x -> xs ++ [f x]) []

lasti :: [a] -> a
lasti = foldr1 (\_ x -> x)
