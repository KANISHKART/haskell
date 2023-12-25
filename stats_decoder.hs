-- PROBLEM 7

import Data.Set as Set
import System.IO  
import Data.Char
import System.Environment
import Data.Char(toLower)

-- IMPORTANT !!!  execute this in cmd  ghci> :set -package containers
-- run the above command to execute the code without error


-- The below code will read all the words in filepath list to a single list
-- This recursion will iterate through each file and read all the words
-- it will return the list with all the words in all files
extractWords :: [FilePath] -> IO [String]
extractWords  [] = return []
extractWords (x:xs) =do
     content <- readFile x
     let itr_words = words content
     readNext <- extractWords(xs)
     return (itr_words ++ readNext)

--Initially I had used removeDuplicates as it has complexity of o(n^2)  
-- it was taking longer time to extract unique words.
-- so I had used set to extract unique words

-- removeDuplicates []=[]
-- removeDuplicates (x:xs)= x: removeDuplicates (filter (/=x) (x:xs))


--below code read 3 novel files and extract a dict.txt file with unique words
-- 1. intializing file path list 
-- 2. invoking extractWords by passing filePath List
-- 3. extracting unique words using Set 
-- 4. writing it to a file
extractUniqueWords :: IO ()
extractUniqueWords=do
    let files=["pride.txt", "dorian.txt","ulysses.txt"]
    wordsList <- extractWords files
    let uniqueList= Set.fromList wordsList
    print("extracting...")
    writeFile "dict.txt"  (unlines (Set.toList uniqueList))
    print "Unique words generated from 3 novels. 'dict.txt' file is created in src code folder"


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c
 
--converting capital to smaller and keep spaces.
clean_input :: String->String
clean_input z= [toLower (x)| x<- z, toLower (x) `elem` ['a'..'z'] || x == ' ']

-- deshift function uses  ((x - n) mod 26) formula to decrypt the words.
-- if there is a alphabet it will decrypt it by above mentioned formula
-- else return the character
deshift :: Int -> Char -> Char
deshift n c
 | isLower c = int2let ((let2int c - n) `mod` 26)
 | otherwise = c


--list comphrension to iterate each word to decode the word
decode :: Int->String ->String
decode n s = [deshift n c | c <- s]

-- decrypt logic which takes encrypt.txt file and index of the cipher 
-- 1. mod the index to 26 before processing so this will eliminate even if the number is more than 26 it will mod and give the result
-- 2. readfile will read encrypted text from .txt file
-- 3. clean input will pre process the file to make the text lower
-- 4. decode function will iterate each word and decrypt the text using deshift
-- 5. writing decrypted text to a file called "decrypt.txt"
decrypt :: FilePath-> Int-> IO ()
decrypt f index=do
    let n= index `mod` 26
    fileContent <- readFile f
    let cleanedText = clean_input fileContent
    let decryptContent = decode n cleanedText
    print "decrypt.txt file generated. check your your src code folder"
    writeFile "decrypt.txt" decryptContent


-- This below list comphrension logic will compare two list 
-- and return length of matching words
countMatchingWords list1 list2=length [x | x<-list1 , x `elem` list2]

-- This below code will iterate each index from 0 to 26
-- 1. it finds matching words to list1 and list2 
-- 2. checks for the maximum matching words by accumulator result 
-- 3. return the tuple with maximum (index, matchedWords).
iterateIndex :: Int -> String -> [String] -> (Int, Int) ->(Int, Int) 
iterateIndex 0 _ _ result = result
iterateIndex n decryptText dictList result =do
    let count = countMatchingWords (words (decode n decryptText)) dictList
    let updatedResult
          | count > (snd result) = (n, count)
          | otherwise = result
    iterateIndex (n - 1) decryptText dictList updatedResult


-- This function is the main logic
-- 1. extract file content in the encrypted file "italy.txt.chp"
-- 2. call extractUniqueWords function to generate "dict.txt" file
-- 3. read contents of dict to a list 
-- 4. call iterateIndex with step 1 , 3
-- 5. decode the "italy.txt.chp" file content with maximum index
-- 6. store the decrypted result in secret.txt file
guess_index :: IO ()
guess_index = do
    fileContent <- readFile "italy.txt.chp"
    let encryptFileText = clean_input fileContent
    extractUniqueWords
    dictContent <- readFile "dict.txt"
    let dictFileList = words dictContent
    print "Iterating 26 index to decrypt file..."
    let result = iterateIndex 26 encryptFileText dictFileList (0,0)
    let decryptContent = decode (fst result) encryptFileText
    writeFile "secret.txt" decryptContent
    print "secret.txt file generated with decrypted message. check src code folder"
       
