-- Kanishkar T (D23124630)

-- Programming Paradigms. 

-- Assignment 2 - HASKELL

-- PROBLEM 10

-- The function integral takes four arguments
-- f = integrand
-- x1 = starting point of intergation interval
-- x2 = ending point og integration interval
-- n = number of intervals to divide the integration range
integral :: (Float -> Float) -> Float -> Float -> Float -> Float
integral f x1 x2 n =
-- 1. calculates the width of each subinterval ('dx') by
-- dividing the total width of the integration interval ('x2-x1') by the number of intervals (n).
  let dx = (x2 - x1) / n
      -- 1. calculates the sum of the function values at each interval point using a right fold ('foldr')
      -- and maps the function f over the interval points.
      -- 2. map (\i -> x1 + i * dx) [0..n-1] =>  This code generates a list of interval points by mapping 
      -- each index i to the corresponding point in the interval  ('x1 + i * dx').
      -- 3. map f .... :- It applies the function  f at each interval point
      -- 4. foldr (+) 0.0: This code sums up the results using a right fold, starting from 0.0
      sum = foldr (+) 0.0 (map f (map (\i -> x1 + i * dx) [0..n-1]))
--5. This in expression calculates the final results by multiplying the width of each subinterval ('dx')
-- with the sum of function values over the intervals. This is based on the concept of approximating the integral as 
-- the sum of areas of rectnagles.
  in dx * sum

-- As mentioned in problem statement
-- It can be changed to any function returning a real number
f :: Float -> Float
f x = 0.5 * x