{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.List

import Control.Applicative hiding (many)
import Control.Monad

import Text.Printf
import Text.Parsec

type TextAdventure = Map String TAFunc

data TAFunc = TAFunc
  { name :: String
  , message :: Maybe String
  , result :: Maybe (Either String [TAOpt])
  }
  deriving (Eq, Show)
  
data TAOpt = TAOpt
  { key :: String
  , desc :: String
  , funcName :: String
  }
  deriving (Eq, Show)

fromListBy f = Map.fromList . map (\x -> (f x, x))

loadAdventure fname = do
  advText <- readFile fname
  case parse advP fname advText of
    Left err -> print err
    Right advList -> runAdventure $ fromListBy name advList

advP = many1 funcP

funcP = do
  n <- nameP
  m <- messageP
  r <- resultP
  newline
  return $ TAFunc n (Just m) (Just $ Right r)

funcNameP = many1 $ choice [upper, char '_']

nameP = do
  char '@'
  space
  n <- funcNameP
  newline
  return n

messageP = do
  char '>'
  space
  m <- manyTill anyChar newline
  return m

resultP = many taOptP
taOptP = do  
  char '-'
  space
  k <- upper
  space
  char '-'
  space
  d <- manyTill anyChar (try $ string " -")
  space
  f <- funcNameP
  newline
  return $ TAOpt [k] d f


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
            Nothing -> return Nothing
            Just (Left f') -> return $ Just f'
            Just (Right []) -> return Nothing
            Just (Right opts) -> Just <$> getOpt opts
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
