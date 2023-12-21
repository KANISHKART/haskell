chk_square x y
    | x < 0 = False
    | x == 1 || x==0 = True
    | y*y ==x = True
    | (y*y > x) = False
    | otherwise = chk_square x (y+1)

is_square x = chk_square x 1
