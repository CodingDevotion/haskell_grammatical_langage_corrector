module Main where

-- Custom imports
import TerminalMenuHelper
import ConnectionHelper (connection)
import CorrectionModes
import GenerateCorrections
import qualified Data.Text.IO as TIO

-- import Data.Hashtable as Hashtable

main :: IO ()
main = do
  printWelcome
  connection
  selectCorrectionMode