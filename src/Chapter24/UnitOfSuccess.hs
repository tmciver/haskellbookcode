module Chapter24.UnitOfSuccess where

import Text.Trifecta

intThenEnd :: Parser Integer
intThenEnd = integer <* eof
