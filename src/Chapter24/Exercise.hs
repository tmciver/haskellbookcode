module Chapter24.Exercise where

import Text.Trifecta

-- Exercise 1
one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneFail :: Parser Char
oneFail = char '1' >> eof >> pure '1'

oneTwoFail :: Parser Char
oneTwoFail = char '1' >> char '2' >> eof >> pure '2'
