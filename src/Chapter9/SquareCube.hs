module Chapter9.SquareCube where

mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

myExp :: [(Integer, Integer)]
myExp = [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]

l = length myExp
