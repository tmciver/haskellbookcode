module Chapter23.StackCalc2 where

data State s a = State { runState :: s -> (a, s) }

type StackCalc a = State [Int] a

instance Functor (State s) where
  fmap fOfA (State fOfS) = State (\s -> let (a, s') = fOfS s
                                            b = fOfA a
                                        in (b, s')
                                 )

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  (State fOfSReturningF) <*> (State fOfS) = State (\s -> let (f, s') = fOfSReturningF s
                                                             (a, s'') = fOfS s'
                                                             b = f a
                                                         in (b, s'')
                                                  )

instance Monad (State s) where
  (State f) >>= g = State (\s -> let (x, s') = f s
                                     (State h) = g x
                                     (y, s'') = h s'
                                 in (y, s'')
                             )

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

push :: Int -> StackCalc ()
push x = modify (x :)

pop :: StackCalc Int
pop = State (\(x:xs) -> (x, xs))

add :: StackCalc ()
add = do
  x <- pop
  y <- pop
  push (x + y)

multiply :: StackCalc ()
multiply = do
  x <- pop
  y <- pop
  push (x * y)

runInt :: StackCalc Int
runInt = do
  push 2
  push 3
  add
  push 3
  multiply
  pop
