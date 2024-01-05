-- Kanishkar T (D23124630)

-- Programming Paradigms. 
-- Assignment 2 - HASKELL

-- PROBLEM 6

import System.IO  
import Data.Char
import System.Environment
import Data.Char(toLower)
main = do  
   [filename,index] <- getArgs
   contents <- readFile filename  
   writeFile (filename++".chp") (encode (read index :: Int) contents)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c
 
encode :: Int -> String -> String
encode n s = [shift n (toLower c) | c <- s]

----------------------------------------------------------------------------------------------------------

--NOTE : Below code contains decrypt logic for ceaser cipher


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

-- main decode logic which takes encrypt.txt file and index of the cipher 
-- this will check if the index is greater than 0 then it will continue by calling decrypt function
-- else throw error messagee

-- TEST with running the code > c_decrypt "encrypt.txt" 7
c_decrypt:: FilePath-> Int->IO()
c_decrypt f index
   | index >=0 = decrypt f index
   | otherwise = print "Enter positive cypher index"

