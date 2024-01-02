-- Kanishkar T (D23124630)

-- Programming Paradigms. 

-- Assignment 2 - HASKELL


-- PROBLEM 3

import Data.Char(toLower)

-- cities database
cities::[(Int,String,Integer,Int)]
cities= [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3),(1,"Edinburgh",500000,2),(1,"Florence",50000,3), (1,"Venice",200000,3), (1,"Lyon",1000000,1),(1,"Milan",3000000,3), (1,"Madrid",6000000,4), (1,"Barcelona",5000000,4)]

-- countires database
countries::[(Int,String)]
countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")] 

-- below function is to extract population 
-- this custom type is created to fetch nth element in a tuple
-- in this case it is used to fetch population in cities database
extractPopulation :: (Int, String , Integer, Int) -> Integer
extractPopulation (_,_,x,_)=x

-- below function is to extract id 
-- this custom type is created to fetch nth element in a tuple
-- in this case it is used to fetch id in cities database
extractId :: (Int, String , Integer, Int) -> Int
extractId (_,_,_,z)=z

-- below function is to extract city 
-- this custom type is created to fetch nth element in a tuple
-- in this case it is used to fetch city in cities database
extractCity :: (Int, String , Integer, Int) -> String
extractCity (_,y,_,_)=y

--(a) 
--below list comphrension is used to fetch all cities above population given by input
--This below code will iterate through each tuple and check population and whther it is above or equal to user input and retrive the results
get_city_above::Integer->[String]
get_city_above n= [extractCity x| x <-cities , let popul=extractPopulation x , popul >= n]

--below list comphrension is used to fetch all cities which matches countries id.
get_cities::Int->[String]
get_cities id= [extractCity x| x <- cities, let temp_id=extractId x, temp_id ==id]

--below code will recursively check in a list of tuple (Int, String) for country string.
--if matches return the id
extract_id :: String->[(Int, String)]->Int
extract_id _ []=0
extract_id city (x:xs)
    | map toLower (snd x) == (map toLower city ) = fst x
    | otherwise = extract_id city (xs)

--(b)
--inner function calls the extract id to fetch the id of the country
--outer function call get_cities to fetch all cities matching country id
get_city::String->[String]
get_city city=get_cities (extract_id city countries)

--(c)
--recursion to count all cities for each country
--input will be countries database and fetches second element in tuple to fetch city
count_city ::[(Int, String)]->[(String, Int)]
count_city []=[]
count_city (x:xs)= (snd x,length(get_city (snd x))): count_city (xs)

--below code calls above count_city for all country by passing countries database as input
num_city::[(String, Int)]
num_city=count_city countries

