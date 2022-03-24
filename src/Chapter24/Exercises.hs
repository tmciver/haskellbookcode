module Chapter24.Exercises where

import Text.Trifecta
import Control.Applicative ((<|>))

-- Exercise 1
one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneFail :: Parser Char
oneFail = char '1' <* eof -- >> pure '1'

oneTwoFail :: Parser Char
oneTwoFail = char '1' >> char '2' >> eof >> pure '2'

-- Exercise 2
p123 :: Parser String
p123 = choice [ try (string "1" <* eof)
              , try (string "12" <* eof)
              , (string "123" <* eof)
              ]
