-- from "Learn You a Haskell"
-- http://learnyouahaskell.com/input-and-output

import System.Environment ( getArgs )
import System.IO
import Control.Exception ( evaluate)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Text.Parsec.String
import Numeric

data Mode = I_OPTION | K_OPTION | S_OPTION | V_OPTION | H_OPTION deriving (Show)

data Params = Params { mode :: Mode, file :: String } deriving (Show)

data Point =  Point { x:: Integer, y::Integer } deriving (Show)

data Curve = Curve {p:: Integer, a::Integer, b::Integer, g::Point, n::Integer, h::Integer} deriving (Show)

main :: IO()
main = do
    args <- getArgs                  -- IO [String]
    let params = argsParse args
    fileContent <- myReadFile params
    let curve = inputParse fileContent
    -- let result = execute params
    print curve

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

-- parseNull :: Parser JSON 
-- parseNull = Null <$ string "null"

-- inputParse:: String -> Curve


-- inputParse:: [] -> error "Zly format"
-- execute :: Params -> String
-- execute (Params I_OPTION x) = "hehe"