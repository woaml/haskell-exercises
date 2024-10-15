-- task 6: Числа Деланнуа
delannoy :: Int -> Int -> Int
delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)

main :: IO ()
main = do
    print $ delannoy 2 2  -- 13
    print $ delannoy 3 3  
    print $ delannoy 4 4