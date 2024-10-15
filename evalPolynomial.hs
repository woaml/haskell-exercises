-- task 7: Вычисление многочлена
evalPolynomial :: [Int] -> Int -> Int
evalPolynomial coeffs x = foldl (\acc a -> acc * x + a) 0 coeffs


main :: IO ()
main = do
  print (evalPolynomial [2, 1, 5] 3) -- 26