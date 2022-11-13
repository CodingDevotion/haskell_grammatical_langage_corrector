module CorrectionModes where

-- Library imports
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashSet as HashSet
import qualified Data.Char as Char
import Prelude hiding (Word)

-- Custom imports
import FileReaderHelper ( readLinesOfFile )
import GenerateCorrections ( computeValidCorrectionsInDic )
import TerminalMenuHelper ( printCorrectionMenu, printToTerminal )
import Types (Color(..), Word, Correction (Correction, originalWord, isCorrectlyWritten, possibleCorrections, isPunctuation), Sentence)
import qualified Data.Map as Map
import CorrectionImplementation

-- Get the option of the user.
selectCorrectionMode :: IO ()
selectCorrectionMode = do
  printCorrectionMenu
  optionSelected <- getLine

  case optionSelected of
    "A" -> startInputTxtFileCorrection
    "B" -> startInteractiveCorrection
    "C" -> existCorrection
    _  ->  printToTerminal Red "\n\nInvalid Input. Please try again!\n" <> selectCorrectionMode

startInputTxtFileCorrection :: IO ()
startInputTxtFileCorrection = do
  dictionarylines <- readLinesOfFile "dict.txt"
  
  let dictionaryOfWords = HashSet.fromList dictionarylines
  userInput <- readLinesOfFile "input.txt"   -- List of all sentences
  let unwordedInput = fmap (T.words . separatePuncFromWords) userInput
  printToTerminal Blue "\n\nHere is the corrected text: \n\n"
  mapM_ (printCorrectedSentence dictionaryOfWords) unwordedInput

  printToTerminal Yellow  "\n\nWhat do you want to do next ?"
  selectCorrectionMode

startInteractiveCorrection :: IO ()
startInteractiveCorrection = do
  printToTerminal White "Enter a sentence you want to correct \n"
  printToTerminal Blue "   For example: \n"
  printToTerminal Cyan "      Alexandre likos his catz \n"

  dictionarylines <- readLinesOfFile "dict.txt"
  let dictionaryOfWords = HashSet.fromList dictionarylines
  
  userInput <- TIO.getLine
  let unwordedInput = (T.words . separatePuncFromWords) userInput
  printToTerminal Blue "\n\nHere is the corrected text: \n\n"
  printCorrectedSentence dictionaryOfWords unwordedInput

  printToTerminal Yellow  "\n\nWhat do you want to do next ?"
  selectCorrectionMode

existCorrection :: IO ()
existCorrection = do
  printToTerminal White "\n\nThank you for using the Super Duper Grammar corrector!\n"
  printToTerminal Yellow "See you soon!"

createCorrectionObject ::HashSet.HashSet Word -> Word -> Correction
createCorrectionObject wordsDictionary word  
  | word `HashSet.member` wordsDictionary || lowerFirstLetter word `HashSet.member` wordsDictionary = Correction {originalWord = word, isCorrectlyWritten = True, possibleCorrections = [], isPunctuation = False}        -- The word is in the dictionary, then no errors
  | isWordPunctuation word = Correction {originalWord = word, isCorrectlyWritten = True, possibleCorrections = [], isPunctuation = True}
  | otherwise = Correction {originalWord = word, isCorrectlyWritten = False, possibleCorrections = computeValidCorrectionsInDic word wordsDictionary, isPunctuation = False}

createSentenceCorrection :: HashSet.HashSet Word -> Sentence -> [(Word, Correction)]
createSentenceCorrection wordsDictionary = map (\word -> (word, createCorrectionObject wordsDictionary word))

printCorrectedSentence ::  HashSet.HashSet Word -> Sentence -> IO ()
printCorrectedSentence  wordsDictionary sentence =
  TIO.putStrLn (T.unwords (fmap (T.pack . show . snd) (createSentenceCorrection wordsDictionary sentence)))

separatePuncFromWords :: Word -> Word
separatePuncFromWords "" = ""
separatePuncFromWords word = 
  if x `T.elem` ",.?!:;\"" then T.cons ' ' (T.cons x  (separatePuncFromWords xs)) else T.cons x (separatePuncFromWords xs)
  where x = T.head word
        xs = T.tail word

isWordPunctuation :: Word -> Bool
isWordPunctuation word = T.length word == 1 && T.elem (T.head word) ",.?!:;\"" 

lowerFirstLetter :: Word -> Word
lowerFirstLetter "" = ""
lowerFirstLetter word = T.cons (Char.toLower $ T.head word) $ T.tail word