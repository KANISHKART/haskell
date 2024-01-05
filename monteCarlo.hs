-- Kanishkar T (D23124630)

-- Programming Paradigms. 
-- Assignment 2 - HASKELL

-- PROBLEM 8

-- For the given graph with quarter circle 
-- radius = 1
-- area of circle = π r^2
-- for full circle area = π * 1 * 1
-- for quarter circle = π / 4
-- area for quarter = 0.785

-- below monteCarlo logic will try to approximate the answer 0.785

-- IMPORTANT !! below one is installed using cabal 
-- "cabal install random"
import System.Random


-- below generate random will generate a float between 0 and 1
-- randomRIO is used to generate the random value between numbers
generateRandom :: IO Float
generateRandom = randomRIO (0,1)


-- after generating radndom values we pass on to calculate 
-- we can say that a point is in circle if both points x,y is <= radius
-- In this case we check whether x & y is <=1 if so return 1 else 0
calculate :: Float->Float->Float
calculate x y
    | ((x^2 + y^2 ) <= 1) = 1
    | otherwise = 0

-- This is main logic where it will run the recursion for n times
-- higher the n value = near to radius approximation
-- this below code will return the sum of all the n values at the end
monteCarlo :: Int-> Float-> IO Float
monteCarlo n total
    | (n>1) = do
        x <- generateRandom
        y <- generateRandom
        monteCarlo (n-1) (total+ (calculate x y))
    | otherwise = return total


-- This is main function which calls monteCarlo function 
-- user can input the iteration and will return the result closest to 0.785 
simulate_radius :: Int -> IO Float
simulate_radius n
    | n>0 =do
    sum <- monteCarlo n 0
    let result = (sum/ fromIntegral n)
    return result
    | otherwise = do 
        print "Enter positive cypher index"
        return 0.0

-- below calculation same as before but this is a static one
-- this will give closest radius to actual radius 0.785
calculate_radius :: IO Float
calculate_radius=do
    sum <- monteCarlo 101000 0
    let result = (sum/ fromIntegral 101000)
    return result




    