{-# LANGUAGE InstanceSigs #-}

module Types where

-- Imports
import Data.List (intercalate)
import qualified Data.Text as T
import Prelude hiding (Word)

-- Type synonyms
type Word = T.Text

type Sentence = [Word]

type PasswordHash = String

type Username = String

-- Data
data Color = Blue | Black | Red | Green | Yellow | Magenta | Cyan | White

data Correction = Correction
  { originalWord :: Word,
    isCorrectlyWritten :: Bool,
    possibleCorrections :: [Word], -- Empty if isCorrectlyWritten == true
    isPunctuation :: Bool
  }

instance Show Correction where
  show :: Correction -> String
  show correction
    | isCorrectlyWritten correction = surroundStringWithColor Green $ T.unpack (originalWord correction)
    | otherwise = surroundStringWithColor Red (T.unpack (originalWord correction) ++ surroundStringWithColor Black (" [" ++ T.unpack (T.intercalate ", " (possibleCorrections correction)) ++ "]"))

surroundStringWithColor :: Color -> String -> String
surroundStringWithColor color textToColorate =
  colorCode ++ textToColorate ++ colorReset
  where
    colorNumber = case color of
      Black -> 0
      Red -> 1
      Green -> 2
      Yellow -> 3
      Blue -> 4
      Magenta -> 5
      Cyan -> 6
      White -> 7
    colorCode = "\x1b[3" ++ show colorNumber ++ "m"
    colorReset = "\x1b[0m"
