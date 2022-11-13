module TerminalMenuHelper where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Custom imports
import Types ( Color(..))

surroundTextWithColor :: Color -> T.Text -> T.Text
surroundTextWithColor color textToColorate =
  colorCode <> textToColorate <> colorReset
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
    colorCode = "\x1b[3" <> T.pack (show colorNumber) <> "m"
    colorReset = "\x1b[0m"


printWelcome :: IO ()
printWelcome = do 
  printToTerminal Yellow "\n\nWelcome to the Super Duper Cool Language Corrector"

printCorrectionMenu :: IO ()
printCorrectionMenu = do
  printToTerminal White "Select what mode you want to enter in: \n"
  printToTerminal Magenta $ "A" <> " - Start correcting the input.txt file"
  printToTerminal Magenta $ "B" <> " - Interactive correction mode"
  printToTerminal Red $ "C" <> " - Exit the program \n"


printToTerminal ::  Color -> T.Text -> IO ()
printToTerminal color textToPrint = do
  TIO.putStrLn $ surroundTextWithColor color textToPrint


