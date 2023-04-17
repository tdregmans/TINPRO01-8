{--
    opdracht2.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-17
--}

-- Opdracht 2.1
data Bintree a = Branch (Bintree a) (Bintree a) | Empty

-- Opdracht 2.2
preorder :: (Bintree a) -> [a]
preorder tree = [] -- return tree in the form of a list

postorder :: (Bintree a) -> [a]
postorder tree = [] -- return tree in the form of a list

inorder :: (Bintree a) -> [a]
inorder tree = [] -- return tree in the form of a list


push :: (Ord a) => (Bintree a) -> a -> (Bintree a)
push tree item = tree -- add code to push item in the tree at the right location

pushlist :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushlist tree items = tree -- add code to push items in the tree at the right location

-- maptree :: (a -> b) -> (Bintree a) -> (Bintree b)
-- maptree function tree = tree -- apply function on all elements of tree

-- filtertree :: (a -> Bool) -> (Bintree a) -> (Bintree b)
-- filtertree bool tree = tree -- return filtered nodes


