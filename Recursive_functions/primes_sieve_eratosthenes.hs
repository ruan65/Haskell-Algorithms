primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (n:ns) = n : sieve [x | x <- ns, x `mod` n /= 0]

