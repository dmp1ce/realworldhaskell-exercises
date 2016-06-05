-- file: ch03/Tree.hs
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

aTree = Node "hello" (Node "another" Empty Empty) Empty

data TreeM a  = NodeM a
                (Maybe (TreeM a))
                (Maybe (TreeM a))
                    deriving (Show)

maybeTree = NodeM "Start" (Just (NodeM "two" Nothing Nothing)) Nothing

-- Exercise 8
height :: Tree a -> Int
height Empty                    = 0
height (Node _ Empty Empty)     = 1
height (Node _ x y)
  | (height x) >= (height y)    = 1 + height x
  | otherwise                   = 1 + height y


heightIs2 = height aTree == 2

longerTree = Node "hello"
  (Node "another" Empty Empty) (Node "another"  -- 2
  (Node "3"                                     -- 3
  (Node "4"                                     -- 4
   Empty Empty) Empty) Empty)

heightIs4 = height longerTree == 4
