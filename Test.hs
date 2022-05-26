import Data.List

prime :: Integral a => a -> Bool
prime n = [x | x <- [1 .. n], n `mod` x == 0 && odd n] == [1, n]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = [x | x <- [0 .. (2 * n + 2)], prime x]

-- Not mine
marked :: Integer -> [Integer]
marked n = [i + j + 2 * (i * j) | i <- [1 .. n], j <- [1 .. n], i + j + 2 * (i * j) <= n && i <= j]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) (filter (\x -> x `notElem` marked n) [1 .. n])

---

remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups [x] = [x]
remdups (x1 : x2 : xs)  | x1 == x2 = remdups (x2 : xs)
                        | otherwise = x1 : remdups (x2 : xs)