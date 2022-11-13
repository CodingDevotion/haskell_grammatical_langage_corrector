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