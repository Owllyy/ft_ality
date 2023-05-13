{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.KeyMap
import Data.String
import GHC.Generics
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import qualified Data.Map as Map

type ActionKey = Char;
type Action = String;

type ActionMap = (Map.Map ActionKey Action);

data Combo = Combo {
  action :: Action,h
  to_state :: String
} deriving (Show, Generic)
instance FromJSON Combo

data State = State {
  combo :: Combo,
  comboTimeout :: String,
  write :: String
} deriving (Show, Generic)
instance FromJSON State

data Config = Config {
  characterName :: String,
  initial :: State,
  keyMap :: ActionMap,
  states :: [State]
} deriving (Show, Generic)
instance FromJSON Config

doAction :: Config -> State -> ActionKey -> String
doAction config state key = case Map.lookup key (keyMap config) of
  Nothing -> show "Invalid Key"
  Just action -> action

run :: Config -> State -> ActionKey -> State
run config state key = case doAction config state tape of
  Left e -> Left $ show e
  Right result -> case result of
    Nothing -> Right $ show tape
    Just (s, t) -> run config s t
    
-- TODO Show help
help :: IO ()
help = putStrLn "Invalid arguments"

-- TODO parse args
main :: IO ()
main = do
  args <- getArgs
  case args of
    path -> do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> putStrLn err
        Right config -> do
          interact run
          case result of
            Left e -> putStrLn $ "Error: " ++ e
            Right r-> putStrLn $ "Success: " ++ r
    _ -> help
