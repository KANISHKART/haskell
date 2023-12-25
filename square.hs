-- PROBLEM 1

--below is a recursion to check number is a sqrt 
--iterating from 1 to sqrt input which user had given
--if multiple itself equals, goes above or sqrt equals iteration will exit the recursion
chk_square x y
    | x < 0 = False
    | x == 1 || x==0 = True
    | y*y ==x = True
    | (y*y > x) = False
    | otherwise = chk_square x (y+1)

is_square x = chk_square x 1
