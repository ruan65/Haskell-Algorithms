ftr :: Int -> Int
ftr 0 = 1;
ftr n = n * ftr (n - 1)