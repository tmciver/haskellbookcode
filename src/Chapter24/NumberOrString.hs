module Chapter24.NumberOrString where

import Text.Trifecta
import Control.Applicative ((<|>))

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Right <$> string "blah") <|> (Left <$> integer)
  -- <|> (Right <$> some letter)

main = do
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
