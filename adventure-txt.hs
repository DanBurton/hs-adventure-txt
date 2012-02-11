{-# LANGUAGE NoMonomorphismRestriction #-}


-- Imports -------------------------------------------------

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Map (Map)

import Data.List (find)
import Control.Applicative ((<$>))
import Text.Printf (printf)
import System.Environment (getArgs)

import Text.Parsec


-- Data types ----------------------------------------------

type TextAdventure = Map String TAFunc

data TAFunc = TAFunc
  { name    :: String
  , message :: Maybe String
  , result  :: TAFuncBody
  } deriving (Eq, Show)

data TAFuncBody = TheEnd
                | FuncCall String
                | Opts [TAOpt]
                deriving (Eq, Show)

data TAOpt = TAOpt
  { key      :: String
  , desc     :: String
  , funcName :: String
  } deriving (Eq, Show)


type Parser a = Parsec String () a


-- Helper Parsers and Parser Combinators -------------------

skip :: Parser a -> Parser ()
skip p = do
  _ <- p
  return ()

char' :: Char -> Parser ()
char' = skip . char

commentP :: Parser ()
commentP = do
  _ <- noneOf " \t\n@-=>"
  _ <- manyTill anyChar newline
  return ()

spaces' :: Parser ()
spaces' = skipMany $ oneOf " \t"

newline' :: Parser ()
newline' = skip newline

spacesAndComments :: Parser ()
spacesAndComments = do
  _ <- many (skipMany1 space <|> commentP)
  return ()


funcNameP :: Parser String
funcNameP = many1 $ choice [upper, char '_']

funcNameSpaces :: Parser String
funcNameSpaces = do
  spaces'
  n <- funcNameP
  spaces'
  newline'
  return n


-- Main Parsers --------------------------------------------

advP :: Parser TextAdventure
advP = fromListBy name <$> many1 funcP

funcP :: Parser TAFunc
funcP = do
  spacesAndComments
  n <- nameP
  spacesAndComments
  m <- optionMaybe messageP
  spacesAndComments
  r <- resultP
  return $ TAFunc n m r


nameP :: Parser String
nameP = do
  char' '@'
  funcNameSpaces


messageP :: Parser String
messageP = do
  char' '>'
  spaces'
  manyTill anyChar newline'


resultP :: Parser TAFuncBody
resultP = option TheEnd ((Opts <$> many1 taOptP) <|> funcCallP)

taOptP :: Parser TAOpt
taOptP = do  
  char' '-'
  spaces'
  k <- upper
  spaces'
  char' '-'
  spaces'
  d <- manyTill anyChar (try $ spaces' >> skip (char '-'))
  spaces'
  f <- funcNameP
  spaces'
  newline'
  return $ TAOpt [k] d f

funcCallP :: Parser TAFuncBody
funcCallP = do
  char' '='
  FuncCall <$> funcNameSpaces


-- Helpers -------------------------------------------------

fromListBy :: Ord k => (v -> k) -> [v] -> Map k v
fromListBy f = Map.fromList . map (\x -> (f x, x))


-- Drivers -------------------------------------------------

main :: IO ()
main = do
  [fname] <- getArgs
  loadAdventure fname


loadAdventure :: String -> IO ()
loadAdventure fname = do
  advText <- readFile fname
  case parse advP fname advText of
    Left err -> print err
    Right adv -> runAdventure adv


runAdventure :: TextAdventure -> IO ()
runAdventure adv = loop "START"
  where
    loop :: String -> IO ()
    loop f = do
      next <- stepAdventure f
      case next of
        Just f' -> loop f'
        Nothing -> askRestart

    stepAdventure :: String -> IO (Maybe String)
    stepAdventure f = case Map.lookup f adv of
      Just (TAFunc _ m r) -> do
        F.forM_ m putStrLn
        case r of
          TheEnd      -> return Nothing
          FuncCall f' -> return $ Just f'
          Opts []     -> return Nothing
          Opts opts   -> Just <$> getOpt opts
      Nothing -> error "Malformed adventure definition. :("

    askRestart :: IO ()
    askRestart = do
      putStrLn "Your adventure has ended. Would you like to restart? (Y/N)"
      c <- getLine
      case c of
        "Y" -> loop "START"
        _   -> putStrLn "See you later, adventurer!"

    getOpt :: [TAOpt] -> IO String
    getOpt opts = do
      mapM_ (\(TAOpt k d _) -> putStrLn $ printf "%s -> %s" k d) opts
      line <- getLine
      case find ((== line) . key) opts of
        Just (TAOpt _ _ f) -> return f
        Nothing -> getOpt opts
