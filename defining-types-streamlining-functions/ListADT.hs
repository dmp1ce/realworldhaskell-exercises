-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Exercise 1
toList :: (List x) -> [x]
toList Nil          = []
toList (Cons x xs)  = x : toList xs

myList = Cons "Hello" $ Cons "Goodbye" Nil

toListTime = toList myList
