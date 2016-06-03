myListOfLists = [[1,2,3],[1,2,3,4],[3,4,5],[1],[5,5,5,5,5],[],[10,2,2,2,2,2,2,2,2,2]]

-- Probably WAY too complicated, but it works to sort
-- lists in decending order of list length.
-- I believe this is a variation of bubble sort.
sortByLength list
  | isSorted list == True     = list
  where
    isSorted []               = True
    isSorted (x:[])           = True
    isSorted (x:y:zs)
      | length x >= length y  = True && isSorted (y:zs)
      | otherwise             = False
sortByLength (x:y:zs)   
  | length x >= length y  =
      sortByLength ([x] ++ sortByLength (y:zs))
  | otherwise             = sortByLength (y:x:zs)

sortedListOfLists = sortByLength myListOfLists
