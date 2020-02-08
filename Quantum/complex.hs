type Complex = (Float,Float)
type Polar   = (Float,Float)

zero :: Complex
zero = (0, 0)

re :: Complex -> Float
re (x, y) = x

im :: Complex -> Float
im (x, y) = y

conjugate :: Complex -> Complex
conjugate (r,i) = (r,-i)

add :: Complex -> Complex -> Complex
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

times :: Complex -> Complex -> Complex
times (x1, y1) (x2, y2) = ((x1 * x2) - (y1 * y2), (x1 * y2) + (y1 * x2))

neg :: Complex -> Complex
neg (x,y) = (-x,-y)

scale :: Complex -> [Complex] -> [Complex]
scale c zs = map (times c) zs

add_vec :: [Complex] -> [Complex] -> [Complex]
add_vec (x:xs) (y:ys) = (add x y) : (add_vec xs ys)

neg_vec :: [Complex] -> [Complex]
neg_vec v = map neg v

zero_vec :: Int -> [Complex]
zero_vec n = replicate n zero

conjugate_vec :: [Complex] -> [Complex]
conjugate_vec v = map conjugate v

add_matrix :: [[Complex]] -> [[Complex]] -> [[Complex]]
add_matrix (x:xs) (y:ys) = (add_vec x y) : (add_matrix xs ys)

neg_matrix :: [[Complex]] -> [[Complex]]
neg_matrix m = map neg_vec m

scale_matrix :: Complex -> [[Complex]] -> [[Complex]]
scale_matrix c m = map (scale c) m

conjugate_matrix :: [[Complex]] -> [[Complex]]
conjugate_matrix m = map conjugate_vec m

get_nth_row_of_transpose :: [[Complex]] -> Int -> Int -> Int -> [Complex]
get_nth_row_of_transpose m num_rows col row = if (row >= num_rows)
                                                 then []
                                                 else (((m !! row) !! col) : (get_nth_row_of_transpose m num_rows col (row + 1)))

transpose_helper :: [[Complex]] -> Int -> Int -> Int -> [[Complex]]
transpose_helper m num_rows num_cols col = if (col >= num_cols)
                                              then []
                                              else ((get_nth_row_of_transpose m num_rows col 0) : (transpose_helper m num_rows num_cols (col + 1)))

transpose :: [[Complex]] -> [[Complex]]
transpose m = transpose_helper m (length m) (length (m !! 0)) 0

divide :: Complex -> Complex -> Complex
divide (x1, y1) (x2, y2) = ((((x1 * x2) + (y1 * y2)) / ((x2 * x2) + (y2 * y2)))
                           ,(((x2 * y1) - (x1 * y2)) / ((x2 * x2) + (y2 * y2))))

modulus :: Complex -> Float
modulus (x, y) = sqrt ((x * x) + (y * y))

angle :: Complex -> Float
angle (x,y) = atan (y / x)

to_polar :: Complex -> Polar
to_polar c = (modulus c , angle c)

polar_mul :: Polar -> Polar -> Polar
polar_mul (r1,t1) (r2,t2) = (r1 * r2 , t1 + t2)

polar_div :: Polar -> Polar -> Polar
polar_div (r1,t1) (r2,t2) = (r1 / r2 , t1 - t2)

polar_pow :: Polar -> Float -> Polar
polar_pow (r,t) n = (r ** n, n * t)

to_complex :: Polar -> Complex
to_complex (r,t) = (r * cos t , r * sin t)

list_nth_roots :: Float -> Float -> (Float -> Float) -> [Polar]
list_nth_roots re_part 0 get_im_part = [(re_part, (get_im_part 0))]
list_nth_roots re_part n get_im_part = (re_part, (get_im_part n)) : (list_nth_roots re_part (n - 1) get_im_part)

nth_root :: Polar -> Float -> [Polar]
nth_root (r,t) n = list_nth_roots (r ** (1 / n)) (n - 1) (\k -> ((1 / n) * (t + (2 * pi * k))))

-- main = print((scale_matrix (1,2) [[(1,-1),(3,0)],[(2,2),(4,1)]]))
-- main = print(scale_matrix (0,2) (scale_matrix (1,2) [[(1,-1),(3,0)],[(2,2),(4,1)]]))
-- main = print(scale_matrix (add (0,2) (1,2)) [[(1,-1),(3,0)],[(2,2),(4,1)]])
main = print(transpose [[(6,-3),(2,12),(0,-19)],[(0,0),(5,2.1),(17,0)],[(1,0),(2,5),(3,-4.5)]])
