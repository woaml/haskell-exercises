-- task 1: Наибольший общий делитель
myGcd :: Int -> Int -> Int
myGcd a b
    | a < 0 || b < 0 = error "Both numbers must be positive"
    | a == 0         = b
    | b == 0         = a
    | a == b         = a
    | a > b          = myGcd (a - b) b
    | otherwise      = myGcd a (b - a)

main :: IO ()
main = do
    print (myGcd 48 18)