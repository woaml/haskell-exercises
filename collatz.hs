-- task 5: Сиракузская последовательность
collatz :: Integer -> Int
collatz n
  | n == 1    = 1
  | even n    = 1 + collatz (n `div` 2)
  | otherwise = 1 + collatz (3 * n + 1)

main :: IO ()
main = do
  print (collatz 7)