-- Kanishkar T (D23124630)

-- Programming Paradigms. 
-- Assignment 2 - HASKELL


--PROBLEM 4

zipping:: [Float]->[Float]->[(Float, Float)]
zipping list1 list2= zip list1 list2

euclidiean_calc :: [(Float, Float)] -> Float
euclidiean_calc []=0
euclidiean_calc (x:xs)= (fst x - snd x) * (fst x- snd x) + euclidiean_calc xs

eucl_dist list1 list2= sqrt(euclidiean_calc(zipping list1 list2))