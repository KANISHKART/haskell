-- Kanishkar T (D23124630)

-- Programming Paradigms. 
-- Assignment 2 - HASKELL


--PROBLEM 4

-- This below method will zip the input list x and y vectors
-- so that it will be easy to calculate x1 ,y1 values in euclidean calculation
-- It form a tuple for each element and returns a list of tuple
zipping:: [Float]->[Float]->[(Float, Float)]
zipping list1 list2= zip list1 list2

-- This below method is where main calculation happens
-- Zipped list will be passed as a paramter
-- And in each element in the input list is a tuple we access the x and y values by fst and snd respectively
euclidiean_calc :: [(Float, Float)] -> Float
euclidiean_calc []=0
euclidiean_calc (x:xs)= (fst x - snd x) * (fst x- snd x) + euclidiean_calc xs


-- This below method will calculate the square root of 
-- after zipping and calculating euclidean distance
-- this method will take a user input and produces the euclidean distance between two lists
-- result in float.
eucl_dist::[Float]->[Float]->Float
eucl_dist list1 list2= sqrt(euclidiean_calc(zipping list1 list2))