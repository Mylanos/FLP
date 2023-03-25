-- from "Learn You a Haskell"
-- http://learnyouahaskell.com/input-and-output

import System.Environment   
import Data.List  

main = do  
    args <- getArgs                  -- IO [String]
    progName <- getProgName          -- IO String
    putStrLn "The arguments are:"  
    mapM putStrLn args  
    putStrLn "The program name is:"  
    putStrLn progName