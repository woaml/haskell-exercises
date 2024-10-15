-- task 9: Сшивание списков бинарной операцией
xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys

main :: IO ()
main = do
    print (xZipWith (+) [10, 20, 30] [9, 8, 7])          -- [19, 28, 37]
    print (xZipWith (+) [10, 20, 30] [9, 8, 7, 6, 5, 4]) -- [19, 28, 37]
    print (xZipWith (+) [10, 20, 30] [])                  -- []