myList = [1,2,3,4]
aString = "race"

reverseElements (x:xs) = (reverseElements xs) ++ [x]
reverseElements []     = []

makePalindrome l  = l ++ reverseElements l
 
myListPalindrome = makePalindrome myList
aStringPalindrome = makePalindrome aString

isPalindrome l = l == reverseElements l 

myListIsPalindrome = isPalindrome myList
myListPalindromeIsPalindrome = isPalindrome myListPalindrome
