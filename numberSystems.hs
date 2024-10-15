-- task 11: Системы счисления
fromDigits :: Int -> [Int] -> Int
fromDigits n digits = sum [d * (n ^ i) | (d, i) <- zip (reverse digits) [0..]]

toDigits :: Int -> Int -> [Int]
toDigits n 0 = [0] 
toDigits n x = reverse $ toDigitsHelper x
  where
    toDigitsHelper 0 = []
    toDigitsHelper num = let (q, r) = num `divMod` n
                         in r : toDigitsHelper q

addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise n xs ys = reverse $ addHelper (reverse xs) (reverse ys) 0
  where
    addHelper [] [] carry = if carry == 0 then [] else [carry]
    addHelper x [] carry = addHelper x [0] carry
    addHelper [] y carry = addHelper [0] y carry
    addHelper (x:xs) (y:ys) carry =
      let sumDigits = x + y + carry
          newCarry = sumDigits `div` n
          digit = sumDigits `mod` n
      in digit : addHelper xs ys newCarry

main :: IO ()
main = do
    print $ fromDigits 2 [1, 0, 1, 1, 0, 1]  -- 45

    print $ toDigits 2 45  -- [1, 0, 1, 1, 0, 1]

    print $ addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1]  -- [1, 1, 0, 1, 0, 0]