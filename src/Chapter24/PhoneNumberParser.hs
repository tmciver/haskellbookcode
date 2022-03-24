module Chapter24.PhoneNumberParser where

import Data.Char (digitToInt)
import Text.Trifecta
import Text.Parser.Char

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> parseNumberingPlanArea
                         <*> parseExchange
                         <*> parseLineNumber

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = undefined

parseExchange :: Parser Exchange
parseExchange = undefined

parseLineNumber :: Parser LineNumber
parseLineNumber = undefined

parseThreeDigits :: (Monad m, CharParsing m)
                 => m (Int, Int, Int)
parseThreeDigits = do
  i <- digitToInt <$> digit
  j <- digitToInt <$> digit
  k <- digitToInt <$> digit
  pure (i, j, k)
