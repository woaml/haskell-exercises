-- task 2: Возведение в степень за O(log n)
power :: Int -> Int -> Int
power _ 0 = 1
power x n
  | n < 0     = error "Negative powers are not supported"
  | even n    = let half = power x (n `div` 2) in half * half
  | otherwise  = x * power x (n - 1)

main :: IO ()
main = do
    print (power 2 10)  -- 1024
    print (power 3 5)   -- 243
    print (power 5 3)   -- 125
    print (power 7 0)   -- 1