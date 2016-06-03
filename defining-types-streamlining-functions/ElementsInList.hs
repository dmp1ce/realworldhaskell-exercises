myList = [1,2,3,4]

numberOfElements :: [a] -> Int 
numberOfElements (x:xs) = 1 + numberOfElements xs
numberOfElements []     = 0

isEqualToLength = numberOfElements myList == length myList
