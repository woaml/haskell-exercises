-- task 4: Совершенные числа
-- Функция для нахождения делителей числа n
divisors :: Integer -> [Integer]
divisors n = filter (\x -> n `mod` x == 0) [1..(n `div` 2)]

-- Функция для проверки, является ли число совершенным
isPerfect :: Integer -> Bool
isPerfect n
  | n <= 0    = False
  | otherwise = sum (divisors n) == n

main :: IO ()
main = do
  print (isPerfect 6)   -- True, 6 = 1 + 2 + 3
  print (isPerfect 28)  -- True, 28 = 1 + 2 + 4 + 7 + 14
  print (isPerfect 12)  -- False, 12 != 1 + 2 + 3 + 4 + 6
  print (isPerfect 496)  -- True, 496 = 1 + 2 + 4 + 8 + 16 + 31 + 62 + 124 + 248