import System.Environment ( getArgs )
import System.IO
import System.Random
import Control.Exception ( evaluate)
import Text.ParserCombinators.Parsec
import Numeric
import Data.Char (intToDigit)

data Mode = I_OPTION | K_OPTION | S_OPTION | V_OPTION deriving (Show)

data Params = Params Mode String deriving (Show)

data Point =  Point Integer Integer | INF_POINT deriving (Eq)

instance Show Point where
  show (Point x y) = decToHex x ++ decToHex y ++ "\n"
  show INF_POINT = "INFINITY POINT\n"

data Curve = Curve Integer Integer Integer Point Integer Integer deriving (Show)

data KeyPair = KeyPair Integer Point

instance Show KeyPair where
  show (KeyPair priv pub) = "Key {\nd: 0x" ++ decToHex priv ++ "\nQ: 0x04" ++ show pub ++ "}"

data Signature = Signature Integer Integer

instance Show Signature where
  show (Signature r s) = "Signature {\nr: 0x" ++ decToHex r ++ "\ns: 0x" ++ decToHex s ++ "\n}"
 
main :: IO()
main = do
    args <- getArgs
    let params = argsParse args
    fileContent <- myReadFile params
    --let numberGenerator = randomNum
    -- get a random number generator:
    let mySeed = 7654321
    let rng1 = mkStdGen mySeed  

    case params of
      (Params I_OPTION _) -> 
        case curveParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right curve -> print curve
      (Params K_OPTION _) -> 
        case curveParseHandler fileContent of 
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right curve -> print $ keypair 
            where 
              (keypair, _) = generateKeyPair curve rng1
      (Params S_OPTION _) -> 
        case signatureCreationParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right (curve, keypair, hash) -> print $ signature 
            where 
              (signature, _) = createSignature curve keypair hash rng1
      (Params V_OPTION _) -> 
        case signatureVerificationParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right (curve, signature, pub, hash) -> print $ result
            where 
              result = signatureVerification curve pub signature hash
      --(Params H_OPTION _) -> print curve

decToHex :: Integer -> String
decToHex n = showIntAtBase 16 intToDigit n ""           

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
  _ <- spaces
  return $ Curve pVal aVal bVal gVal nVal hVal

splitHex :: String -> (Integer, Integer)
splitHex hex =
  let halfLen = length hex `div` 2
      (left, right) = splitAt halfLen hex
      leftInt = read ("0x" ++ left)
      rightInt = read ("0x" ++ right)
  in (leftInt, rightInt)

secFormatParse :: Parser (Integer, Integer)
secFormatParse = do
  digits <- try (string "0x04" >> many1 hexDigit) <|> many1 digit
  let (x, y) = splitHex digits
  -- return $ fst $ head $ readHex $ digits
  return (x, y)

-- Parse a point
keyParse :: Parser KeyPair
keyParse = do
  _ <- string "Key" >> spaces >> char '{' >> spaces
  dVal <- string "d:" >> spaces >> hexInteger 
  _ <- spaces
  (x, y) <- string "Q:" >> spaces >> secFormatParse
  _ <- spaces
  _ <- string "}" 
  _ <- spaces
  return $ KeyPair dVal (Point x y)

-- Parse a point
signatureParse :: Parser Signature
signatureParse = do
  _ <- string "Signature" >> spaces >> char '{' >> spaces
  rVal <- string "r:" >> spaces >> hexInteger 
  _ <- spaces
  sVal <- string "s:" >> spaces >> hexInteger
  _ <- spaces
  _ <- string "}" 
  _ <- spaces
  return $ Signature rVal sVal

-- Parse a point
publicKeyParse :: Parser Point
publicKeyParse = do
  _ <- string "PublicKey" >> spaces >> char '{' >> spaces
  (x, y) <- string "Q:" >> spaces >> secFormatParse
  _ <- spaces
  _ <- string "}" 
  _ <- spaces
  return $ Point x y

hashParse :: Parser Integer
hashParse = do 
  _ <- spaces
  hashVal <- string "Hash:" >> spaces >> hexInteger 
  _ <- spaces
  _ <- spaces
  return hashVal

-- parsing
curveParseHandler :: String -> Either ParseError Curve
curveParseHandler input = parse curveParse "" input

signatureCreationParseHandler :: String -> Either ParseError (Curve, KeyPair, Integer)
signatureCreationParseHandler input = parse combinedParser "" input
  where 
    combinedParser = do
      curve <- curveParse
      keyPair <- keyParse
      hash <- hashParse
      return (curve, keyPair, hash)

signatureVerificationParseHandler :: String -> Either ParseError (Curve, Signature, Point, Integer)
signatureVerificationParseHandler input = parse combinedParser "" input
  where 
    combinedParser = do
      curve <- curveParse
      signature <- signatureParse
      publicKey <- publicKeyParse
      hash <- hashParse
      return (curve, signature, publicKey, hash)

-- [0, 0] + [2, 3] -> OK [2,3]
-- [2, 3] + [0, 0] -> OK [2,3]
-- [2, 3] + [1, 0] -> OK [2,3]
pointDoubleOrAdd :: Curve -> Point -> Point -> Point
pointDoubleOrAdd _ INF_POINT p2 = p2
pointDoubleOrAdd _ p1 INF_POINT = p1
pointDoubleOrAdd _ (Point 0 0) p2 = p2
pointDoubleOrAdd _ p1 (Point 0 0) = p1
pointDoubleOrAdd (Curve p a _ _ _ _) p1@(Point x1 y1) p2@(Point x2 y2)
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
pointMultiply _ _ INF_POINT = INF_POINT 
pointMultiply c n p@(Point _ _) 
-- addition - firstly call recursively pointMultiply and then add the result to current point
  | n `mod` 2 == 1 = pointDoubleOrAdd c p $ pointMultiply c (n-1) p
-- doubling - firstly double the current point and then recursively call pointMultiply
  | otherwise = pointMultiply c (n `div` 2) $ pointDoubleOrAdd c p p

modInv :: Integer -> Integer -> Integer
modInv a m = 
  let (r, t, _) = extendedEuclidean a m
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
generateKeyPair c@(Curve _ _ _ k n _) rng = 
  let 
    (secretKey, rng2) = randomizeInt n rng
  in (KeyPair secretKey $ pointMultiply c secretKey k, rng2)

createSignature :: RandomGen tg => Curve -> KeyPair -> Integer -> tg -> (Signature, tg)
createSignature (Curve _ _ _ INF_POINT _ _) _ _ rng = (Signature 0 0, rng)
createSignature _ (KeyPair _ INF_POINT) _ rng = (Signature 0 0, rng)
createSignature c@(Curve _ _ _ g n _) kp@(KeyPair priv _) hash rng =
  let
    (k, rng2) = randomizeInt n rng
    pointsAdded = pointMultiply c k g
    r = case pointsAdded of
      INF_POINT -> 0
      (Point xkG _ ) -> xkG `mod` n
  in 
    if r == 0 
      then 
        -- recursively try generate again 
        createSignature c kp hash rng2 
      else  
        (Signature r ((hash + r * priv) * (modInv k n) `mod` n), rng2)

isOnCurve :: Curve -> Point -> Bool
isOnCurve _ INF_POINT = False
isOnCurve (Curve p a b _ _ _) (Point x y) =
  let
      left = (y * y) `mod` p
      right = ((x * x * x) + (a * x) + b) `mod` p
  in left == right

isInRange :: Integer -> Integer -> Bool
isInRange upBound n = (1 <= n) && (n <= upBound - 1)

isSignatureInRange :: Signature -> Integer -> Bool
isSignatureInRange (Signature r s) n = 
  let 
    rCheck = isInRange n r
    sCheck = isInRange n s
  in rCheck && sCheck

signatureVerification :: Curve -> Point -> Signature -> Integer -> Bool
signatureVerification (Curve _ _ _ INF_POINT _ _) _ _ _ = False
signatureVerification _ INF_POINT _ _ = False
signatureVerification c@(Curve p _ _ g n _) publicKey sig@(Signature r s) hash =
  let
    onCurve = isOnCurve c publicKey
    result = if onCurve == True 
      then 
        let
          check1 = isSignatureInRange sig n
          w = (modInv s n) `mod` n
          u1 = (hash * w) `mod` n
          u2 = (r * w) `mod` n
          u1G = pointMultiply c u1 g 
          u2Q = pointMultiply c u2 publicKey 
          pointsAdded = pointDoubleOrAdd c u1G u2Q
          check2 = 
            case pointsAdded of
              INF_POINT -> False
              (Point resX _ ) -> 
                ((resX `mod` p) `mod` n) == (r `mod` n)
        in check1 && check2 
      else 
        False
  in result
