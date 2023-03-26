--lenn :: a -> b
--ll :: n -> [a] -> 
lenn :: [a] -> Int

lenn [] = 0
lenn (_:xs) = 
    let ll n [] = n
        ll n (_:xs) = ll (n+1) xs
    in ll 1 xs

main :: IO ()
main =  do 
    putStrLn "Length of list: "
    print(lenn [1..20])

