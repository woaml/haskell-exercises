-- task 10: Список чисел Фибоначчи
fibonacci :: Int -> [Int]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

generalizedFibonacci :: [Int] -> [Int]
generalizedFibonacci initial = initial ++ generate (length initial) initial
  where
    generate m xs = let next = sum (take m xs)
                    in next : generate m (tail xs ++ [next])

main :: IO ()
main = do
    -- Первые 10 чисел Фибоначчи
    print $ fibonacci 10  -- [0,1,1,2,3,5,8,13,21,34]

    -- Бесконечный список чисел Фибоначчи
    print $ take 10 fibs   -- [0,1,1,2,3,5,8,13,21,34]

    -- Бесконечный список обобщённых чисел Фибоначчи
    print $ take 10 $ generalizedFibonacci [7, 3, 10, 0]  -- [7,3,10,0,20,33,63,106,222,424]