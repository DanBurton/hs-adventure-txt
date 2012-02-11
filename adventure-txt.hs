{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List

import Control.Applicative hiding (many, (<|>))
import Control.Monad

import Text.Printf
import Text.Parsec

import System.Environment


spaces' = skipMany $ oneOf " \t"

type TextAdventure = Map String TAFunc

data TAFuncBody = TheEnd
                | FuncCall String
                | Opts [TAOpt]
                deriving (Eq, Show)

data TAFunc = TAFunc
  { name :: String
  , message :: Maybe String
  , result :: TAFuncBody
  } deriving (Eq, Show)

data TAOpt = TAOpt
  { key :: String
  , desc :: String
  , funcName :: String
  } deriving (Eq, Show)

fromListBy f = Map.fromList . map (\x -> (f x, x))

loadAdventure fname = do
  advText <- readFile fname
  case parse advP fname advText of
    Left err -> print err
    Right advList -> runAdventure $ fromListBy name advList

spacesAndComments = many (skipMany1 space <|> commentP)

commentP = do
  noneOf " \t\n@-=>"
  manyTill anyChar newline
  return ()

main = do
  [fname] <- getArgs
  loadAdventure fname

advP = many1 funcP

funcP = do
  spacesAndComments
  n <- nameP
  spacesAndComments
  m <- optionMaybe messageP
  spacesAndComments
  r <- resultP
  return $ TAFunc n m r

funcNameP = many1 $ choice [upper, char '_']

nameP = do
  char '@'
  spaces'
  n <- funcNameP
  spaces'
  newline
  return n

messageP = do
  char '>'
  spaces'
  m <- manyTill anyChar newline
  return m

resultP = option TheEnd ((Opts <$> many1 taOptP) <|> funcCallP)

taOptP = do  
  char '-'
  spaces'
  k <- upper
  spaces'
  char '-'
  spaces'
  d <- manyTill anyChar (try $ spaces' >> char '-')
  spaces'
  f <- funcNameP
  spaces'
  newline
  return $ TAOpt [k] d f

funcCallP = do
  char '='
  spaces'
  f <- funcNameP
  spaces'
  newline
  return $ FuncCall f

runAdventure :: TextAdventure -> IO ()
runAdventure adv = loop "START"
  where
    loop f = do
      next <- stepAdventure f
      case next of
        Just f' -> loop f'
        Nothing -> askRestart
    stepAdventure f = do
      case Map.lookup f adv of
        Just (TAFunc n m r) -> do
          when (isJust m) $ putStrLn (fromJust m)
          case r of
            TheEnd      -> return Nothing
            FuncCall f' -> return $ Just f'
            Opts []     -> return Nothing
            Opts opts   -> Just <$> getOpt opts
        Nothing -> error "Malformed adventure definition. :("
    askRestart = do
      putStrLn "Your adventure has ended. Would you like to restart? (Y/N)"
      choice <- getLine
      case choice of
        "Y" -> loop "START"
        _   -> putStrLn "See you later, adventurer!"
    getOpt opts = do
      mapM_ (\(TAOpt k d _) -> putStrLn $ printf "%s -> %s" k d) opts
      line <- getLine
      case find ((== line) . key) opts of
        Just (TAOpt _ _ f) -> return f
        Nothing -> getOpt opts
