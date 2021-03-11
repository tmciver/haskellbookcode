module Chapter9.Filtering where

-- #1
v1 = filter (\x -> mod x 3 == 0) [1..30]

-- #2
f xs = length . filter (\x -> mod x 3 == 0) $ xs

-- #3
myFilter s = filter (\word -> not (elem word ["a", "an", "the"])) (words s)
