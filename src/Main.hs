-- from "Learn You a Haskell"
-- http://learnyouahaskell.com/input-and-output

import System.Environment ( getArgs )
import System.IO
import System.Random
import Control.Exception ( evaluate)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Text.Parsec.String
import Numeric
--import Control.Monad.Random

-- TODO mozno by bolo dobre vracat either aj pre argumenty namiesto tych errorov a potom switchovat na zaklade left right
data Mode = I_OPTION | K_OPTION | S_OPTION | V_OPTION | H_OPTION deriving (Show)

data Params = Params { mode :: Mode, file :: String } deriving (Show)

data Point =  Point { x:: Integer, y::Integer } | INF_POINT deriving (Eq, Show)

data Curve = Curve {p:: Integer, a::Integer, b::Integer, g::Point, n::Integer, h::Integer} deriving (Show)

data KeyPair = KeyPair { private:: Point, public:: Point} deriving (Show)

main :: IO()
main = do
    args <- getArgs                  -- IO [String]
    let params = argsParse args
    fileContent <- myReadFile params
    --let numberGenerator = randomNum
    -- get a random number generator:
    let mySeed = 54321
    let rng1 = mkStdGen mySeed  

    let point1 = Point 0 1
    print $ pointAdd point1 $ pointNegation(point1)


    case inputParse fileContent of
      Left err -> putStrLn $ "Parsing error: " ++ show err
      Right curve ->
        case params of
          (Params I_OPTION _) -> print curve
          (Params K_OPTION _) -> print keypair
            where (keypair, _) = generateKeyPair curve rng1
            
    -- let result = execute params
    --print curve

myReadFile :: Params -> IO String
myReadFile (Params _ file)
    | file /= "" = withFile file ReadMode $ \handle -> do   -- read from file
        content <- hGetContents handle
        _ <- evaluate $ length content -- forcing read of content to avoid delayed read on closed handle error
        return content
    | otherwise = do    -- read stdin
        getContents


-- parse argumentov
argsParse :: [String] -> Params
argsParse [] = error "Nedostatecny pocet argumentov"
argsParse [x]
    | x == "-i" = Params I_OPTION ""
    | x == "-k" = Params K_OPTION ""
    | x == "-s" = Params S_OPTION ""
    | x == "-v" = Params V_OPTION ""
    | otherwise = error "Neznamy argument"
argsParse (_:_:_:_) = error "Prilis vela argumentov"
argsParse (x:y:[])
    | (x == "-i" ) && (y /= "-k") && (y /= "-s") && (y /= "-v" ) = Params I_OPTION y
    | (x == "-k" ) && (y /= "-i") && (y /= "-s") && (y /= "-v" ) = Params K_OPTION y
    | (x == "-s" ) && (y /= "-k") && (y /= "-i") && (y /= "-v" ) = Params S_OPTION y
    | (x == "-v" ) && (y /= "-k") && (y /= "-s") && (y /= "-i" ) = Params V_OPTION y
    | otherwise = error "Zla kombinacia argumentov"

-- Parse a hexadecimal integer
hexInteger :: Parser Integer
hexInteger = do
  digits <- try (string "0x" *> many1 hexDigit) <|> many1 digit
  --digits <- many1 hexDigit
  return $ fst $ head $ readHex $ digits

-- Parse a point
point :: Parser Point
point = do
  string "Point" *> spaces *> char '{' *> spaces
  xVal <- string "x:" *> spaces *> hexInteger <* spaces
  yVal <- string "y:" *> spaces *> hexInteger <* spaces
  string "}" 
  spaces
  return $ Point xVal yVal

eol :: Parser Char
eol = char '\n'

-- Parse a curve
curve :: Parser Curve
curve = do
  string "Curve" *> spaces *> char '{' *> spaces
  pVal <- string "p:" *> spaces *> hexInteger <* spaces
  aVal <- string "a:" *> spaces *> hexInteger <* spaces
  bVal <- string "b:" *> spaces *> hexInteger <* spaces
  gVal <- string "g:" *> spaces *> point
  nVal <- string "n:" *> spaces *> hexInteger <* spaces
  hVal <- string "h:" *> spaces *> hexInteger <* spaces
  string "}"
  return $ Curve pVal aVal bVal gVal nVal hVal

-- Test function
inputParse :: String -> Either ParseError Curve
inputParse input = parse curve "" input


pointNegation :: Point -> Point
pointNegation (Point x y) = (Point x negY)
  where negY = negate(y)

pointAdd :: Point -> Point -> Point
pointAdd p1@(Point x1 y1) INF_POINT = p1
pointAdd INF_POINT p2@(Point x1 y1) = p2
pointAdd p1@(Point x1 y1) p2@(Point x2 y2) 
  | p1 == pointNegation(p2) = INF_POINT


randomizeInt :: RandomGen tg => Integer -> tg -> (Integer, tg)
randomizeInt 0 rng = (0, rng)
randomizeInt n rng = randomNum 1 n rng

randomNum:: RandomGen tg => Integer -> Integer -> tg -> (Integer, tg)
randomNum from to rng = randomR(from, to) rng

generateKeyPair :: RandomGen tg => Curve -> tg -> (Integer, tg)
--generateKeyPair (Curve p a b g n h) rng = KeyPair (Point $ randomNum n $ randomNum n) (Point $ randomNum n $ randomNum n)
generateKeyPair (Curve p a b g n h) rng = 
  let (secretKey, rng2) = randomizeInt n rng
  in (secretKey,rng2)

