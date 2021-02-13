data Tree a = Leaf a | Node (Tree a) a (Tree a)

--a)
instance Show a => Show (Tree a) where
    show (Leaf x) = show x
    show (Node left root right) = show "(" ++ show left ++ show "<" ++ show root ++ show ">" ++ show right ++ show ")"

x = Node (Leaf 1) (2) (Leaf 3)
y = Node x 4 (Leaf 1)--x
z = Node y 5 (Leaf 1)--y


--b)
instance Foldable Tree where
    foldr f z (Leaf x) = f x z
    foldr f z (Node left root right) = foldr f (f root (foldr f z right)) left

--c)
--count root
countr (Leaf x) racc = racc
countr (Node r x l) racc = racc + (countr r racc) + 1 + (countr l racc)

--count leaves
countl (Leaf x) lacc = lacc + 1
countl (Node r x l) lacc = lacc + (countl r lacc) + (countl l lacc)

--contains x
containsx x tree = foldr (\a b -> a==x || b) False tree

--tree height
height (Leaf x) = 1
height (Node r x l) = 1 + max (height l) (height r)

