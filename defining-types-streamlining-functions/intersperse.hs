-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]
intersperse _ []                = []
intersperse _ (x:[])            = x
intersperse seperator (x:xs)    = x ++ [seperator] ++
  (intersperse seperator xs)

myList = [[1,2,3], [2,3,4], [111,222,333,66]]
myStrings = ["hello", "moto"]

interspersedList = intersperse 0 myList
interspersedList2 = intersperse ',' myStrings
