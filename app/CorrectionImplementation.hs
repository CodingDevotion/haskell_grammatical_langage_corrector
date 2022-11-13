{-# LANGUAGE InstanceSigs #-}
module CorrectionImplementation where
-- This module is used to implement Correction functions such as Show. 
-- The Show implementation could not be done in the Types.hs file directly because we would encounter a dependency cycle with
-- TerminalMenuHelper (surroundStringWithColor) and Types (Color). Implementing Correction functions in a separate module is a possible solution to ensure that 
-- the surroundStringWithColor function is not duplicated and that no dependeny cycles are found.
import qualified Data.Text as T
import Types (Color(..), Word, Correction (Correction, originalWord, isCorrectlyWritten, possibleCorrections, isPunctuation))
import TerminalMenuHelper (surroundTextWithColor)

instance Show Correction where
  show :: Correction -> String
  show correction
    | isCorrectlyWritten correction = T.unpack $ surroundTextWithColor Green $ originalWord correction
    | otherwise = T.unpack $ surroundTextWithColor Red (originalWord correction) <> surroundTextWithColor Black (" [" <> T.intercalate ", " (possibleCorrections correction) <> "]")