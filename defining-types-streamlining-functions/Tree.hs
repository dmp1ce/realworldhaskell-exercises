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
