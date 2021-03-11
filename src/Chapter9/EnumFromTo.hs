module Chapter9.EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool b _ = [b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2
  | o1 > o2 = []
  | o1 == o2 = [o1]
  | otherwise = o1 : eftOrd (succ o1) o2
