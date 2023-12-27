-- Kanishkar T (D23124630)

-- Programming Paradigms. 
-- Assignment 2 - HASKELL

--PROBLEM 5

import Data.Char(toLower)

-- NOTE !!!!!
-- Below code of countOf, CountOccurences, freq_letter_pc is same as problem 2 I have used those to extract frequency.
-- after the sepration the code for langFinder is written.

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

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

--making a dictionary for english by storing characters and frequency
--zip is used to join the characters to respective float
eng_dict :: [(Char,Float)]
eng_dict= zip ['a'..'z'] eng_freq 

--making a dictionary for portuguese by storing characters and frequency
--zip is used to join the characters to respective float
port_dict :: [(Char,Float)]
port_dict= zip ['a'..'z'] pt_freq 

--getFrequency is used to fetch the value in eng_dict / port_dict
--iterating through the list to fetch the element and if found returning the same
-- for ex: getFrequency end_dict 'a' will return 8.12 as a response
getFrequency:: [(Char,Float)]-> Char -> Float
getFrequency (x:xs) ele
    | fst x == ele = snd x
    | otherwise = getFrequency (xs) ele

--This below function will iterate through the frequency derived from userInput text
--and it will try to return a list by each character by comparing frequency of the eng_dict , port_dict 
--it will return a list of characters with either 'p' or 'e' which then later will be used to analyse
--whether there is more portugal letters or english letters
findClosest :: [(Char,Float)]->[(Char)]->[(Char)]
findClosest [] a=a
findClosest (x:xs) a
    | abs((getFrequency eng_dict (fst x))-(snd x)) <= abs((getFrequency port_dict (fst x))-(snd x)) = findClosest (xs) ('e':a)
    | otherwise = findClosest (xs) ('p':a)

--This below function is used to determine whether the language is english or portuguese
--Input for this function will be from findClosest output 
--for ex: fetchLanguage "pppppeeepppe" => in this input the below function will count which has higher frequency 'e' or 'p'
--based on this it will give the answer as english or portuguese
fetchLanguage :: String->String
fetchLanguage l
    | getFrequency (freq_letter_pc l) 'e' >= getFrequency (freq_letter_pc l) 'p'="The text is in English"
    | otherwise = "The text is in Portuguese"


--the below two functions counFrequency & freq_letter will return the same as (problem 2)
--but this will return the frequency in terms of percentage
countFrequency :: String->Int->[(Char,Float)]
countFrequency [] _=[]
countFrequency (x:xs) lengthOf = (x, (fromIntegral (countOf x (x:xs))/fromIntegral (lengthOf)  ) * 100): countFrequency (filter (/=x) (x:xs)) lengthOf

freq_letter :: String->[(Char,Float)]
freq_letter z=countFrequency (map toLower (filter (/=' ') z )) (length (filter (/=' ') z ))


--Main code to extract the text and find the langauge
--1. clean_input will extract only the alphabets 
--2. freq_letter is used to calculate the frequency in terms of percentage 
--3. findClosest will map each character in the text to english (e) and portuguese (p) and form a string with p & e
--4. fetchLanguage will count from previous input whether character 'e' is higher or 'p' depending on  it will return the language of the text.
get_lang text=fetchLanguage (findClosest (freq_letter (clean_input text)) [])

--This below code will 
--1. take file as a input.
--2. clean the text by keeping only alphabets.
--3. return the language detected. 
read_lang :: FilePath-> IO String
read_lang f=do
    fileContent <- readFile f
    let cleanedText= clean_input fileContent
    return (get_lang cleanedText)


--clean the input text by removing special characters and space 
--return only alphabets
clean_input :: String->String
clean_input z= [x| x<- z, x `elem` ['a'..'z']]







