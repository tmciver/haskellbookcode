module Chapter9.ThyFearfulSymmetry where

myWords :: String -> [String]
myWords [] = []
myWords s = word : myWords rest
  where word = takeWhile (/= ' ') s
        rest = dropWhile (== ' ') . dropWhile (/= ' ') $ s
