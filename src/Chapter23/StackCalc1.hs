module Chapter23.StackCalc1 where

data StackCalc = StackCalc [Int]

empty :: StackCalc
empty = StackCalc []

push :: Int -> StackCalc -> StackCalc
push x (StackCalc xs) = StackCalc (x:xs)

pop :: StackCalc -> (Int, StackCalc)
pop (StackCalc (x:xs)) = (x, StackCalc xs)

add :: StackCalc -> StackCalc
add (StackCalc (x:y:xs)) = StackCalc ((x + y):xs)

multiply :: StackCalc -> StackCalc
multiply (StackCalc (x:y:xs)) = StackCalc ((x * y):xs)

calc :: Int
calc = let sc1 = push 2 empty
           sc2 = push 3 sc1
           sc3 = add sc2
           sc4 = push 3 sc3
           sc5 = multiply sc4
           (x, _) = pop sc5
       in x
