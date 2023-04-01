import System.Environment ( getArgs )
import System.IO
import System.Random
import Control.Exception ( evaluate)
import Text.ParserCombinators.Parsec
import Numeric

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

    let point1 = Point 6 1
    let point2 = Point 8 1

    -- print $ pointAdd point1 $ pointNegation(point1)
    --print $ pointAdd point1 point2

    case inputParse fileContent of
      Left err -> putStrLn $ "Parsing error: " ++ show err
      Right curve ->
        case params of
          (Params I_OPTION _) -> print curve
          --(Params K_OPTION _) -> print keypair
          --(Params K_OPTION _) -> print $ pointAdd curve point1 point2
          (Params K_OPTION _) -> print $ pointAdd (Curve 37 0 7 (Point 6 1) 0 0) point1 point2
            where (keypair, _) = generateKeyPair curve rng1
          (Params S_OPTION _) -> print curve
          (Params V_OPTION _) -> print curve
          (Params H_OPTION _) -> print curve
            
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
pointParse :: Parser Point
pointParse = do
  _ <- string "Point" *> spaces *> char '{' *> spaces
  xVal <- string "x:" *> spaces *> hexInteger <* spaces
  yVal <- string "y:" *> spaces *> hexInteger <* spaces
  _ <- string "}" 
  spaces
  return $ Point xVal yVal

-- Parse a curve
curveParse :: Parser Curve
curveParse = do
  _ <- string "Curve" *> spaces *> char '{' *> spaces
  pVal <- string "p:" *> spaces *> hexInteger <* spaces
  aVal <- string "a:" *> spaces *> hexInteger <* spaces
  bVal <- string "b:" *> spaces *> hexInteger <* spaces
  gVal <- string "g:" *> spaces *> pointParse
  nVal <- string "n:" *> spaces *> hexInteger <* spaces
  hVal <- string "h:" *> spaces *> hexInteger <* spaces
  _ <- string "}"
  return $ Curve pVal aVal bVal gVal nVal hVal

-- parsing
inputParse :: String -> Either ParseError Curve
inputParse input = parse curveParse "" input

-- [0, 0] + [2, 3] -> OK [2,3]
-- [2, 3] + [0, 0] -> OK [2,3]
-- [2, 3] + [1, 0] -> OK [2,3]
pointAdd :: Curve -> Point -> Point -> Point
pointAdd _ INF_POINT p2 = p2
pointAdd _ p1 INF_POINT = p1
pointAdd (Curve p a b _ n _) p1@(Point x1 y1) p2@(Point x2 y2)
  | (x1 == x2) && (y1 == negate y2) = INF_POINT
  | otherwise = 
  let 
      lambda = pointDouble a p p1 p2
      xNew = ((lambda * lambda) - (x1 + x2)) `mod` p
      yNew = ((lambda * (x1 - xNew)) - y1) `mod` p
  in Point xNew yNew


pointDouble :: Integer -> Integer -> Point -> Point -> Integer
pointDouble _ _ INF_POINT _ = 0
pointDouble _ _ _ INF_POINT = 0
pointDouble a p (Point x1 y1) (Point x2 y2)
  | (x1 == x2) && (y1 == y2) = 
    let 
      res1 = ((3 * ( x2*x2 )) + a) `div` (2 * y2)
    in res1 `mod` p
  | otherwise =
    let 
      yDiff = y2 - y1
      xDiff = x2 - x1
      res2 = yDiff `div` xDiff
    in res2 `mod` p

randomizeInt :: RandomGen tg => Integer -> tg -> (Integer, tg)
randomizeInt 0 rng = (0, rng)
randomizeInt n rng = randomNum 1 n rng

randomNum:: RandomGen tg => Integer -> Integer -> tg -> (Integer, tg)
randomNum from to rng = randomR(from, to) rng

generateKeyPair :: RandomGen tg => Curve -> tg -> (Integer, tg)
--generateKeyPair (Curve p a b g n h) rng = KeyPair (Point $ randomNum n $ randomNum n) (Point $ randomNum n $ randomNum n)
generateKeyPair (Curve _ _ _ _ n _) rng = 
  let (secretKey, rng2) = randomizeInt n rng
  in (secretKey, rng2)

