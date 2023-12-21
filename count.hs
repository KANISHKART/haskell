import Data.Char(toLower)

-- This below function is used to calculate the count of a given char in a string
-- This is a recursive function which will iterate each element until is empty and check whether the element is a alphabet
-- At the end it will return the count
countOf :: Char -> String -> Int
countOf _ []=0
countOf y (x:xs) 
    | y == x  && y `elem` ['a'..'z']= 1 + countOf y (xs)
    | otherwise = countOf y (xs)

-- This below function will take input string , integer
-- This is a recursive function 
-- 1. If string is empty return empty list
-- 2. else call countOf to get the count of character
-- 3. after fetching the count it is divided by length input to get frequency
-- 4. round that of 1 decimal point by using inbuilt round function 
-- 5. repeat the tail with same process by removing all the occurences of that character and send the remaining string so duplication is removed.
countOccurrences :: String->Int->[(Char,Float)]
countOccurrences [] _=[]
countOccurrences (x:xs) lengthOf = (x, fromIntegral(round((fromIntegral (countOf x (x:xs)) / fromIntegral(lengthOf)) * 10)) /10.0 ): countOccurrences (filter (/=x) (x:xs)) lengthOf


-- This below function will take input string and call countOccurrences recursive function to get output in list of tuple
-- processing of input is done before sending it to countOccurrences namely : 
-- 1. converting to lower by using inbuilt toLower function
-- 2. trimming the spaces in the input
-- 3. sending the length of string to countOccurrences to calculate frequency
freq_letter_pc :: String->[(Char,Float)]
freq_letter_pc z=countOccurrences (map toLower (filter (/=' ') z )) (length (filter (/=' ') z ))

