module FileReaderHelper where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readLinesOfFile :: FilePath -> IO [T.Text]
readLinesOfFile filePath = do
  f <- TIO.readFile filePath
  let linesOfFile = T.lines f
      linesOfFileWithValidCharacters = map trimInvalidCharacters linesOfFile
  return linesOfFileWithValidCharacters

trimInvalidCharacters :: T.Text -> T.Text
trimInvalidCharacters = T.reverse . T.dropWhile (== '\r') . T.reverse