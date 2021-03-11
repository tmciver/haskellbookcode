module Chapter9.MoreBottoms where

import Data.Bool (bool)

-- #4
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- #5
x = map (^2) [1..10] -- x = [1,4,9,16,25,36,49,64,81,100]

-- #6
y = map (\x -> bool x (-x) (x == 3)) [1..10]

