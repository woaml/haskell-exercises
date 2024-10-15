-- task 3: Вычисление чисел Фибоначчи за O(log n)
import Data.Matrix

multiplyMatrices :: Matrix Int -> Matrix Int -> Matrix Int
multiplyMatrices a b = fromLists [[a ! (1, 1) * b ! (1, 1) + a ! (1, 2) * b ! (2, 1),
                                    a ! (1, 1) * b ! (1, 2) + a ! (1, 2) * b ! (2, 2)],
                                   [a ! (2, 1) * b ! (1, 1) + a ! (2, 2) * b ! (2, 1),
                                    a ! (2, 1) * b ! (1, 2) + a ! (2, 2) * b ! (2, 2)]]

fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fiboMatrix ^ (n - 1)) ! (2, 2)
  where
    fiboMatrix = fromLists [[0, 1], [1, 1]]

main :: IO ()
main = do
    print $ fibonacci 0   -- 0
    print $ fibonacci 1   -- 1
    print $ fibonacci 2   -- 1
    print $ fibonacci 3   -- 2
    print $ fibonacci 4   -- 3
    print $ fibonacci 9   -- 34
    print $ fibonacci 10  -- 55
    print $ fibonacci 50  -- 12586269025