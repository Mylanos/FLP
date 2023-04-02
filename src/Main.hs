--  ECDSA projekt 
--    xziska03
--   Marek Ziska
--      2023

import System.Environment ( getArgs )
import System.IO (withFile, hGetContents)
import System.IO (IOMode(ReadMode))
import System.Random (RandomGen, randomR, mkStdGen)
import Control.Exception ( evaluate)
import Text.ParserCombinators.Parsec (ParseError, Parser, parse, spaces, string, char, digit, many1)
import Text.Parsec ((<|>), hexDigit, try)
import Numeric (readHex, showIntAtBase)
import Data.Char (intToDigit)

-- data structure to hold mode, determined by given argument
data Mode = I_OPTION | K_OPTION | S_OPTION | V_OPTION deriving (Show)

-- data structure to hold passed arguments
data Params = Params Mode String deriving (Show)

-- data structure that holds Point coordinates x y
data Point =  Point Integer Integer | INF_POINT deriving (Eq)

-- custom Show Point
instance Show Point where
  show (Point x y) = "Point {\nx: 0x" ++ decToHex x ++ "\ny: 0x" ++ decToHex y ++ "\n" ++ "}"
  show INF_POINT = "INFINITY POINT\n"

-- data structure that holds ECDSA Curve parametres
data Curve = Curve Integer Integer Integer Point Integer Integer

-- custom Show Curve
instance Show Curve where
  show (Curve p a b g n h) = "Curve {\np: 0x" ++ decToHex p ++ "\na: " ++ show a ++ "\nb: " ++ show b ++ "\ng: " ++ show g ++ "\nn: 0x" ++ decToHex n ++ "\nh: " ++ show h ++"\n}"

-- data structure that holds keypair of private and public key
data KeyPair = KeyPair Integer Point

-- custom Show KeyPair
instance Show KeyPair where
  show (KeyPair priv (Point x y)) = "Key {\nd: 0x" ++ decToHex priv ++ "\nQ: 0x04" ++ decToHex x ++ decToHex y ++ "\n" ++ "}"
  show (KeyPair priv INF_POINT) = "Key {\nd: 0x" ++ decToHex priv ++ "\nQ: INFINITY POINT\n" ++ "}"

-- data structure that holds generated signature
data Signature = Signature Integer Integer

-- custom Show Signature
instance Show Signature where
  show (Signature r s) = "Signature {\nr: 0x" ++ decToHex r ++ "\ns: 0x" ++ decToHex s ++ "\n}"
 
-- main function, orchestrates the program
main :: IO()
main = do
    -- arguments parsing
    args <- getArgs
    let params = argsParse args
    -- reads file contents
    fileContent <- myReadFile params
    -- inicialization of a random generator
    let mySeed = 7654321
    let rng1 = mkStdGen mySeed  

    case params of
      (Params I_OPTION _) -> 
        -- print curve params
        case curveParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right curve -> print curve
      (Params K_OPTION _) -> 
        -- print generated keypair
        case curveParseHandler fileContent of 
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right curve -> print $ keypair 
            where 
              (keypair, _) = generateKeyPair curve rng1
      (Params S_OPTION _) -> 
        -- print generated signature
        case signatureCreationParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right (curve, keypair, hash) -> print $ signature 
            where 
              (signature, _) = createSignature curve keypair hash rng1
      (Params V_OPTION _) -> 
        -- print outcome of signature verification
        case signatureVerificationParseHandler fileContent of
          Left err -> putStrLn $ "Parsing error: " ++ show err
          Right (curve, signature, pub, hash) -> print $ result
            where 
              result = signatureVerification curve pub signature hash

-- converts decimal integer into string hexadecimal representation
decToHex :: Integer -> String
decToHex n = showIntAtBase 16 intToDigit n ""           

-- reads from stdin or opens and reads a file, depending on passed argument/s
myReadFile :: Params -> IO String
myReadFile (Params _ file)
    -- read from file
    | file /= "" = withFile file ReadMode $ \handle -> do 
        content <- hGetContents handle
        _ <- evaluate $ length content
        return content
    -- read stdin
    | otherwise = do   
        getContents


-- argument parsing
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

-- parsing a hexadecimal number and converting it to integer representation
hexInteger :: Parser Integer
hexInteger = do
  digits <- try (string "0x" >> many1 hexDigit) <|> many1 digit
  return $ fst $ head $ readHex $ digits

-- parsing a point 
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

-- parsing curve parametres
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

-- splits string into two halves of the same size and converts them to integers
-- used to read the public key data
splitHex :: String -> (Integer, Integer)
splitHex hex =
  let halfLen = length hex `div` 2
      (left, right) = splitAt halfLen hex
      leftInt = read ("0x" ++ left)
      rightInt = read ("0x" ++ right)
  in (leftInt, rightInt)

-- parsing the value of public key according to the SEC public key format
secFormatParse :: Parser (Integer, Integer)
secFormatParse = do
  digits <- try (string "0x04" >> many1 hexDigit) <|> many1 digit
  let (x, y) = splitHex digits
  return (x, y)

-- parsing point
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

-- parsing signature
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

-- parsing public key
publicKeyParse :: Parser Point
publicKeyParse = do
  _ <- string "PublicKey" >> spaces >> char '{' >> spaces
  (x, y) <- string "Q:" >> spaces >> secFormatParse
  _ <- spaces
  _ <- string "}" 
  _ <- spaces
  return $ Point x y

-- parsing hash
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

-- handles parsing of input, that is expected in signature creation mode
signatureCreationParseHandler :: String -> Either ParseError (Curve, KeyPair, Integer)
signatureCreationParseHandler input = parse combinedParser "" input
  where 
    combinedParser = do
      curve <- curveParse
      keyPair <- keyParse
      hash <- hashParse
      return (curve, keyPair, hash)

-- handles parsing of input, that is expected in signature verification mode
signatureVerificationParseHandler :: String -> Either ParseError (Curve, Signature, Point, Integer)
signatureVerificationParseHandler input = parse combinedParser "" input
  where 
    combinedParser = do
      curve <- curveParse
      signature <- signatureParse
      publicKey <- publicKeyParse
      hash <- hashParse
      return (curve, signature, publicKey, hash)

-- function that handles addition of two points on curve
-- in case the same point is passed twice, the function behaves like point doubling function
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

-- helping function for the pointAddOrDouble, uses different formulas for equal points and different points
calcLambda :: Integer -> Integer -> Point -> Point -> Integer
calcLambda _ _ INF_POINT _ = 0
calcLambda _ _ _ INF_POINT = 0
calcLambda a p (Point x1 y1) (Point x2 y2)
  -- case the points are the same, use different formula
  | (x1 == x2) && (y1 == y2) = 
    let 
      nominator = ((3 * ( x2*x2 )) + a)
      modInvDenom1 = (modInv (2 * y2) p)
      res1 = nominator * modInvDenom1
    in res1 `mod` p
  -- case the points are not the same, use standard formula
  | otherwise =
    let 
      yDiff = y2 - y1
      xDiff = x2 - x1
      modInvDenom2 = modInv xDiff p
      res2 = yDiff * modInvDenom2
    in res2 `mod` p

-- function calculates n*P multiplication of point by repeating addition and doubling of a given point
pointMultiply :: Curve -> Integer -> Point -> Point
pointMultiply _ 0 _ = INF_POINT
pointMultiply _ 1 p = p 
pointMultiply _ _ INF_POINT = INF_POINT 
pointMultiply c n p@(Point _ _) 
  -- addition - in case of odd number, call recursively pointMultiply and then add the result to current point
  | n `mod` 2 == 1 = pointDoubleOrAdd c p $ pointMultiply c (n-1) p
  -- doubling - in case of even number, double the current point and then recursively call pointMultiply
  | otherwise = pointMultiply c (n `div` 2) $ pointDoubleOrAdd c p p

-- function that determines the modular inverse of given numbers, uses extendedEuclidean algo to find GCD
modInv :: Integer -> Integer -> Integer
modInv a m = 
  let (r, t, _) = extendedEuclidean a m
  in if r /= 1
    then error "There was an error calculating modular inverse"
    else t `mod` m

-- function that searches for greates common divisors
extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean a 0 = (a, 1, 0)
extendedEuclidean a b =
    let 
      (d, c) = divMod a b
      (r, t, s) = extendedEuclidean b c
    in 
      (r, s, t - (d * s))

-- generates random number with given generator and generates a new generator aswell for future use
randomizeInt :: RandomGen tg => Integer -> tg -> (Integer, tg)
randomizeInt 0 rng = (0, rng)
randomizeInt n rng = randomNum 1 n rng

-- generates random number from given range
randomNum:: RandomGen tg => Integer -> Integer -> tg -> (Integer, tg)
randomNum from to rng = randomR(from, to) rng

-- generates KeyPair
generateKeyPair :: RandomGen tg => Curve -> tg -> (KeyPair, tg)
generateKeyPair c@(Curve _ _ _ k n _) rng = 
  let 
    (secretKey, rng2) = randomizeInt n rng
  in (KeyPair secretKey $ pointMultiply c secretKey k, rng2)

-- creates signature
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

-- checks if a point is on given Curve
isOnCurve :: Curve -> Point -> Bool
isOnCurve _ INF_POINT = False
isOnCurve (Curve p a b _ _ _) (Point x y) =
  let
      left = (y * y) `mod` p
      right = ((x * x * x) + (a * x) + b) `mod` p
  in left == right

-- checks if given number is in range of <1, n-1>
isInRange :: Integer -> Integer -> Bool
isInRange upBound n = (1 <= n) && (n <= upBound - 1)

-- calls isInRange for both parametres of signature
isSignatureInRange :: Signature -> Integer -> Bool
isSignatureInRange (Signature r s) n = 
  let 
    rCheck = isInRange n r
    sCheck = isInRange n s
  in rCheck && sCheck

-- verifies validity of a signature with known formulas and return the decision
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
