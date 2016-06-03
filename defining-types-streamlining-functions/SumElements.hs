myList = [1,2,3,4]

meanOfElements x  =
  (sumElements x) / (fromIntegral(length x))
  where
    sumElements (x:xs)  = x + sumElements xs
    sumElements []      = 0

myMean = meanOfElements myList
