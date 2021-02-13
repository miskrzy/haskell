--created datatype
data Tree a = Leaf a | Node (Tree a) a (Tree a)

--instance of Show allowing to print datatype to the console
instance Show a => Show (Tree a) where
    show (Leaf x) = show x
    show (Node left root right) = show "(" ++ show left ++ show "<" ++ show root ++ show ">" ++ show right ++ show ")"

x = Node (Leaf 1) (2) (Leaf 3)
y = Node x 4 (Leaf 1)--x
z = Node y 5 (Leaf 1)--y


--instance of Foldable allowing to map the tree
instance Foldable Tree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node left root right) = foldr f (f root (foldr f z right)) left

--counting roots in given tree
countr (Leaf x) racc = racc
countr (Node r x l) racc = racc + (countr r racc) + 1 + (countr l racc)

--counting leaves in given tree
countl (Leaf x) lacc = lacc + 1
countl (Node r x l) lacc = lacc + (countl r lacc) + (countl l lacc)

--checking if tree contains a certain element
containsx x tree = foldr (\a b -> a==x || b) False tree

--finding tree's hight
height (Leaf x) = 1
height (Node r x l) = 1 + max (height l) (height r)

