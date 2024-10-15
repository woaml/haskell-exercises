-- task 12: Перечисление путей в решётке
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = pathsHelper 0 0 [] a b
  where
    pathsHelper x y currentPath a b
      | x == a && y == b = [currentPath]
      | x > a || y > b = []
      | otherwise = 
          concat [
            pathsHelper (x + 1) y (currentPath ++ [0]) a b,  -- Движение вправо
            pathsHelper x (y + 1) (currentPath ++ [2]) a b,  -- Движение вверх
            pathsHelper (x + 1) (y + 1) (currentPath ++ [1]) a b  -- Диагональное движение
          ]

main :: IO ()
main = do
    let result = delannoyPaths 2 2
    mapM_ print result