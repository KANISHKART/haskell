-- Kanishkar T (D23124630)

-- Programming Paradigms.
 
-- Assignment 2 - HASKELL

-- PROBLEM 9

-- In This below function I have defined a math series
-- This one is the sample series which was given in the problem
sample_series1 :: Float -> Float
sample_series1 k = 1 / (2 ** k)

-- In This below function I have defined a math series
-- This one is the formula which was given in the question
sample_series2 :: Float -> Float
sample_series2 k = ((-1) ** (k+1)) * (4/ ((2*k)-1))


-- This below one is for sample series1 
-- because there wasn't any range mentioned in the problem statement for function input
-- like starting from 0 or 1 but only the end value is mentioned
math_series1 :: (Float -> Float) -> Int -> Float
math_series1 series n = sum (map series [0.0 .. fromIntegral n])


-- This below one is the main solution
-- the math_series uses HOF it maps the series to list of all values and add everything
--sum :: Num a => [a] -> a ; sum takes list of numeric value and sums it up
--map :: (a->b) => [a]->[b] ; map applies a function to each element of a list and returs a new list
math_series :: (Float -> Float) -> Int -> Float
math_series series n = sum (map series [1.0 .. fromIntegral n])

--to fix the range issue I had written a logic will work both ways 
--I have added the check for positive value and it will work for both 0 and 1
compute_math_series :: (Float -> Float) -> (Int, Float) -> Float
compute_math_series series (start, end)
  | start < 0 = error "Start index should be greater than 0"
  | otherwise = sum (map series [fromIntegral start .. end])


-- below function is to show as n increases it closes to π .
main :: IO()
main = do
    let pi_iterations= map (\k -> math_series sample_series2 (10^k)) [1..5]
    mapM_ (\(k, approx) -> putStrLn $ "On k = 10^" ++ show k ++ " , result => " ++ show approx) (zip [1..] pi_iterations)




