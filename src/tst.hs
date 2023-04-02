--lenn :: a -> b
--ll :: n -> [a] -> 


isInRange :: Integer -> Integer -> Bool
isInRange upBound n = (1 <= n) && (n <= upBound - 1)

main :: IO ()
main =  do 
    putStrLn "Length of list: "
    print(isInRange 2 1)

