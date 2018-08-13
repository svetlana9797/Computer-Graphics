data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Read,Show)
  
tree = (Node 1 
              (Node 2 
                  (Node 5 Empty Empty)
                   (Node 4 Empty Empty))
              (Node 3 
                  (Node 7 (Node 8 (Node 9 Empty Empty) Empty) Empty) 
                  (Node 6 Empty Empty)))

--finds the number of knots in a tree
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize tree@(Node a left right) = 1 + hl + hr where
  hl = treeSize left; hr = treeSize right

--finds the sum of all knots
treeSum::(Num a)=>Tree a -> a
treeSum Empty = 0
treeSum t@(Node a left right) = a + treeSum left + treeSum right

--finds the max sum in a path
maxSumPath::(Num a,Ord a) => Tree a->a
maxSumPath Empty =0
maxSumPath tree@(Node a left right) = a + max (maxSumPath left) (maxSumPath right)

--removes the leaves from the tree t
prune::Tree a->Tree a
prune Empty = Empty
prune (Node a Empty Empty) = Empty
prune (Node a left right) = (Node a (prune left) (prune right))

--from tree to list
treeToList::Tree a -> [a]
treeToList Empty = []
treeToList (Node a Empty Empty) = [a]
treeToList (Node a left right) = (treeToList left) ++ [a] ++ (treeToList right)

--adds 2 children to the leaves with thee same value
bloom::Tree a->Tree a
bloom Empty = Empty
bloom (Node a Empty Empty) = (Node a (Node a Empty Empty) (Node a Empty Empty))
bloom (Node a left right) = (Node a (bloom left) (bloom right))

tree2 = bloom tree


