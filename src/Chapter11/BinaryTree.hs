module Chapter11.BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-- filling in some details to help you along
-- Note, you do *not* need to use insert' for this.
-- Retain the original structure of the tree.
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

main :: IO ()
main = do print testTree
          print $ mapTree' (+1) testTree
-- main = if mapTree' (+1) testTree' == mapExpected
--           then print "yup okay!"
--           else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) = preorder l ++ [x] ++ preorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l x r) = preorder l ++ preorder r ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "postorder failed check"

-- main :: IO ()
-- main = do
--   testPreorder
--   testInorder
--   testPostorder

-- any traversal order is fine (I've chose inorder traversal here)
foldTree :: (b -> a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l x r) = f bl x br
  where bl = foldTree f z l
        br = foldTree f z r

-- rewrite `mapTree` using `foldTree`
mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree g Leaf bt
  where --g :: BinaryTree b1 -> a1 -> BinaryTree b1 -> BinaryTree b1
        g l x r = Node l (f x) r

-- main :: IO ()
-- main = print $ foldTree (+) 0 testTree'
