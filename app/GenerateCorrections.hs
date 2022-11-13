module GenerateCorrections where

import qualified Data.Text as T
import Prelude hiding (Word)
import qualified Data.HashSet as HashSet

-- Custom imports
import Types (Word)


alphabet :: [Char]
alphabet = ['a' .. 'z']

generateAllPossibleWordsWithReplacedChar :: Word -> [Word]
generateAllPossibleWordsWithReplacedChar "" = []
generateAllPossibleWordsWithReplacedChar word = [ T.cons a xs | a <- alphabet, a /= x ] <> map (T.cons x) (generateAllPossibleWordsWithReplacedChar xs)
  where x = T.head word
        xs = T.tail word


generateAllPossibleWordsWithAddedAlphabetChar :: Word -> [Word]
generateAllPossibleWordsWithAddedAlphabetChar word = [ T.take index word <> T.cons a (T.takeEnd (lengthWord - index) word) | index <- [0..lengthWord], a <- alphabet]
  where lengthWord = T.length word


generateAllPossibleWordsWithForgottenAlphabetChar :: Word -> [Word]
generateAllPossibleWordsWithForgottenAlphabetChar word = [ T.take index word <> T.takeEnd (lengthWord - index - 1) word | index <- [0..(lengthWord - 1)]]
  where lengthWord = T.length word


generateAllPossibleWordsWithSwitchedPairOfChar :: Word -> [Word]
generateAllPossibleWordsWithSwitchedPairOfChar word = [ wordWithSwitchedPair | index <- [0..(lengthWord - 2)]
  , let wordWithSwitchedPair = T.take index word <> T.cons (T.index word (index + 1)) (T.cons (T.index word index) (T.takeEnd (lengthWord - index - 2) word)) 
  , wordWithSwitchedPair /= word]
  where lengthWord = T.length word


generateAllCombinaisonWordsWithAppliedFunc :: (Word -> Word) -> Word -> [Word]
generateAllCombinaisonWordsWithAppliedFunc f word = [ newWord  | index <- [0..(lengthWord - 1)]
  , let newWord = T.take index word <> f (T.cons (T.index word index) "") <> T.takeEnd (lengthWord - index - 1) word
  , newWord /= word]
  where lengthWord = T.length word

generateAllPossibleWordsWithLowerCapitalLetters :: Word -> [Word]
generateAllPossibleWordsWithLowerCapitalLetters = generateAllCombinaisonWordsWithAppliedFunc T.toLower

generateAllPossibleWordsWithCapitalLetters :: Word -> [Word]
generateAllPossibleWordsWithCapitalLetters = generateAllCombinaisonWordsWithAppliedFunc T.toUpper



  -- Return all the possible way of writing a word.
computeAllPossibleCorrections :: Word -> [Word]
computeAllPossibleCorrections word = generateAllPossibleWordsWithReplacedChar word 
                                    ++ generateAllPossibleWordsWithAddedAlphabetChar word
                                    ++ generateAllPossibleWordsWithForgottenAlphabetChar word
                                    ++ generateAllPossibleWordsWithSwitchedPairOfChar word
                                    ++ generateAllPossibleWordsWithLowerCapitalLetters word
                                    ++ generateAllPossibleWordsWithCapitalLetters word

-- Helper functions
splitIfPunc :: Word -> [Word]
splitIfPunc = T.split (`T.elem` ",.?!-:;\"\'")

-- By converting the list into a Set, be make sure to remove the duplicates.
-- The operation is in O(n) because every member of the list needs to be converted.
-- It would be interesting to find a quicker time complexity solution.
removeDuplicates :: [Word] -> [Word]
removeDuplicates = HashSet.toList . HashSet.fromList


-- Return a Word with all the possible correction that are stored in the valid words hashset.
-- For a single Word, compute all the variation of this word and return only the once that are present in the word dictionary.
computeValidCorrectionsInDic :: Word -> HashSet.HashSet Word -> [Word]
computeValidCorrectionsInDic word wordsDictionary = filter (`HashSet.member` wordsDictionary) $ removeDuplicates $ computeAllPossibleCorrections word

