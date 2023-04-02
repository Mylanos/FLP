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

data KeyPair = KeyPair { private:: Integer, public:: Point} deriving (Show)

data Signature = Signature { r:: Integer, s:: Integer} deriving (Show)


main :: IO()
main = do
    args <- getArgs                  -- IO [String]
    let params = argsParse args
    fileContent <- myReadFile params
    --let numberGenerator = randomNum
    -- get a random number generator:
    let mySeed = 7654321
    let rng1 = mkStdGen mySeed  

    let point1 = Point 6 1
    let point2 = Point 8 1
    let point3 = Point 13 16
    let hash = 67213401616213387970971763789012420154960832235626142256921887584606087023075
    let publicKey = Point 46708116978045949128126606656587981811337507042416885728414175564892275890406 5702285058721776178091486875671758702564598994857583307601405313313922439896
    -- print $ pointDoubleOrAdd point1 $ pointNegation(point1)
    --print $ pointDoubleOrAdd point1 point2

    case inputParse fileContent of
      Left err -> putStrLn $ "Parsing error: " ++ show err
      Right curve ->
        case params of
          (Params I_OPTION _) -> print curve
          --(Params K_OPTION _) -> print $ extendedEuclidean 240 46
          --(Params K_OPTION _) -> print $ pointDoubleOrAdd curve point1 point2
          --(Params K_OPTION _) -> print $ pointDoubleOrAdd (Curve 37 0 7 (Point 6 1) 7 1) point1 point1
          (Params K_OPTION _) -> print $ keypair
            where 
              (keypair, _) = generateKeyPair curve rng1
          (Params S_OPTION _) -> print $ signature 
            where 
              (keypair, rng2) = generateKeyPair curve rng1
              (signature, _) = createSignature curve keypair hash rng2
          (Params V_OPTION _) -> print $ result
            where 
              (keypair, rng2) = generateKeyPair curve rng1
              (signature, _) = createSignature curve keypair hash rng2
              result = signatureVerification curve keypair signature hash
          --(Params H_OPTION _) -> print curve
            
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
  digits <- try (string "0x" >> many1 hexDigit) <|> many1 digit
  --digits <- many1 hexDigit
  return $ fst $ head $ readHex $ digits

-- Parse a point
pointParse :: Parser Point
pointParse = do
  _ <- string "Point" >> spaces >> char '{' >> spaces
  xVal <- string "x:" >> spaces >> hexInteger 
  _ <- spaces
  yVal <- string "y:" >> spaces >> hexInteger
  _ <- spaces
  _ <- string "}" 
  spaces
  return $ Point xVal yVal

-- Parse a curve
curveParse :: Parser Curve
curveParse = do
  _ <- string "Curve" >> spaces >> char '{' >> spaces
  pVal <- string "p:" >> spaces >> hexInteger 
  _ <- spaces
  aVal <- string "a:" >> spaces >> hexInteger
  _ <- spaces
  bVal <- string "b:" >> spaces >> hexInteger
  _ <- spaces
  gVal <- string "g:" >> spaces >> pointParse
  nVal <- string "n:" >> spaces >> hexInteger
  _ <- spaces
  hVal <- string "h:" >> spaces >> hexInteger
  _ <- spaces
  _ <- string "}"
  return $ Curve pVal aVal bVal gVal nVal hVal

-- parsing
inputParse :: String -> Either ParseError Curve
inputParse input = parse curveParse "" input

-- [0, 0] + [2, 3] -> OK [2,3]
-- [2, 3] + [0, 0] -> OK [2,3]
-- [2, 3] + [1, 0] -> OK [2,3]
pointDoubleOrAdd :: Curve -> Point -> Point -> Point
pointDoubleOrAdd _ INF_POINT p2 = p2
pointDoubleOrAdd _ p1 INF_POINT = p1
pointDoubleOrAdd _ (Point 0 0) p2 = p2
pointDoubleOrAdd _ p1 (Point 0 0) = p1
pointDoubleOrAdd (Curve p a b _ n _) p1@(Point x1 y1) p2@(Point x2 y2)
  | (x1 == x2) && (y1 == negate y2) = INF_POINT
  | otherwise = 
  let 
      lambda = calcLambda a p p1 p2
      xNew = ((lambda * lambda) - (x1 + x2)) `mod` p
      yNew = ((lambda * (x1 - xNew)) - y1) `mod` p
  in Point xNew yNew

calcLambda :: Integer -> Integer -> Point -> Point -> Integer
calcLambda _ _ INF_POINT _ = 0
calcLambda _ _ _ INF_POINT = 0
calcLambda a p (Point x1 y1) (Point x2 y2)
  -- case the points are the same
  | (x1 == x2) && (y1 == y2) = 
    let 
      nominator = ((3 * ( x2*x2 )) + a)
      modInvDenom1 = (modInv (2 * y2) p)
      res1 = nominator * modInvDenom1
    in res1 `mod` p
  | otherwise =
    let 
      yDiff = y2 - y1
      xDiff = x2 - x1
      modInvDenom2 = modInv xDiff p
      res2 = yDiff * modInvDenom2
    in res2 `mod` p

pointMultiply :: Curve -> Integer -> Point -> Point
pointMultiply _ 0 _ = INF_POINT
pointMultiply _ 1 p = p 
pointMultiply c n p@(Point _ _) 
-- addition - firstly call recursively pointMultiply and then add the result to current point
  | n `mod` 2 == 1 = pointDoubleOrAdd c p $ pointMultiply c (n-1) p
-- doubling - firstly double the current point and then recursively call pointMultiply
  | otherwise = pointMultiply c  (n `div` 2) $ pointDoubleOrAdd c p p

modInv :: Integer -> Integer -> Integer
modInv a m = 
  let (r, t, s) = extendedEuclidean a m
  in if r /= 1
    then error "There was an error calculating modular inverse"
    else t `mod` m

extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean a 0 = (a, 1, 0)
extendedEuclidean a b =
    let (r, t, s) = extendedEuclidean b c
    in (r, s, t - (d * s))
     where 
      (d, c) = divMod a b

randomizeInt :: RandomGen tg => Integer -> tg -> (Integer, tg)
randomizeInt 0 rng = (0, rng)
randomizeInt n rng = randomNum 1 n rng

randomNum:: RandomGen tg => Integer -> Integer -> tg -> (Integer, tg)
randomNum from to rng = randomR(from, to) rng

generateKeyPair :: RandomGen tg => Curve -> tg -> (KeyPair, tg)
--generateKeyPair (Curve p a b g n h) rng = KeyPair (Point $ randomNum n $ randomNum n) (Point $ randomNum n $ randomNum n)
generateKeyPair c@(Curve _ _ _ k n _) rng = 
  let 
    (secretKey, rng2) = randomizeInt n rng
  -- in (secretKey, rng2) = 
  -- in (KeyPair 91305095057638279798210088207290086814184648949849354342409048655369161716366 $ pointMultiply c 91305095057638279798210088207290086814184648949849354342409048655369161716366 k, rng2)
  in (KeyPair secretKey $ pointMultiply c secretKey k, rng2)

createSignature :: RandomGen tg => Curve -> KeyPair -> Integer -> tg -> (Signature, tg)
createSignature c@(Curve _ _ _ (Point x y) n _) kp@(KeyPair priv pub) hash rng =
  let
    (k, rng2) = randomizeInt n rng
    (Point xkG ykG) = pointMultiply c k (Point x y)
    r = x `mod` n
  in 
    if r == 0 
      then 
        -- recursively try generate again 
        createSignature c kp hash rng2 
      else  
        (Signature r ((hash + r * priv) * (modInv k n) `mod` n), rng2)

isOnCurve :: Curve -> Point -> Bool
isOnCurve c@(Curve p a b _ _ _) (Point x y) =
  let
      left = (y * y) `mod` p
      right = ((x * x * x) + (a * x) + b) `mod` p
  in left == right

signatureVerification :: Curve -> KeyPair -> Signature -> hash -> Bool
signatureVerification _ kp@(KeyPair priv INF_POINT) _ _ = False
signatureVerification c@(Curve _ _ _ (Point x y) n _) kp@(KeyPair priv pub) sig@(Signature r s) hash =
  let
    onCurve = isOnCurve c pub
    result = if onCurve == True 
      then 
        let
          kek = True
        in kek
      else 
        False
  in result

--signatureNotFake = 
