import Data.Int

type Complex = (Float,Float)
type Polar   = (Float,Float)

-- Complex
zero :: Complex
zero = (0, 0)

one :: Complex
one = (1,0)

i :: Complex
i = (0,1)

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

-- Vectors
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

vec_mul :: [Complex] -> [Complex] -> Complex
vec_mul [] [] = zero
vec_mul (v : vs) (w : ws) = add (times v w) (vec_mul vs ws)

-- Matrices
get_num_rows :: [[Complex]] -> Int
get_num_rows m = length m

get_num_cols :: [[Complex]] -> Int
get_num_cols m = length (m !! 0)

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
transpose m = transpose_helper m (get_num_rows m) (get_num_cols m) 0

adjoint :: [[Complex]] -> [[Complex]]
adjoint m = transpose (conjugate_matrix m)

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

get_row :: [[Complex]] -> Int -> [Complex]
get_row (a:d) 0 = a
get_row (a:d) n = get_row d (n - 1)

get_column_helper :: [[Complex]] -> Int -> Int -> Int -> [Complex]
get_column_helper m col cur_row num_rows = if (cur_row >= num_rows)
                                              then []
                                              else (((m !! cur_row) !! col) : (get_column_helper m col (cur_row + 1) num_rows))

get_column :: [[Complex]] -> Int -> [Complex]
get_column m col = get_column_helper m col 0 (get_num_rows m)

matrix_mult_get_row :: [[Complex]] -> [[Complex]] -> Int -> Int -> Int -> Int -> [Complex]
matrix_mult_get_row m1 m2 cur_row cur_col max_rows max_cols =
  if (cur_col >= max_cols)
     then []
     else ((vec_mul (get_row m1 cur_row) (get_column m2 cur_col)) : (matrix_mult_get_row m1 m2 cur_row (cur_col + 1) max_rows max_cols))

matrix_mult_helper :: [[Complex]] -> [[Complex]] -> Int -> Int -> Int -> [[Complex]]
matrix_mult_helper m1 m2 cur_row max_rows max_cols =
  if (cur_row >= max_rows)
     then []
     else ((matrix_mult_get_row m1 m2 cur_row 0 max_rows max_cols) : (matrix_mult_helper m1 m2 (cur_row + 1) max_rows max_cols))

matrix_mult :: [[Complex]] -> [[Complex]] -> [[Complex]]
matrix_mult m1 m2 = matrix_mult_helper m1 m2 0 (get_num_rows m1) (get_num_cols m2)

gen_id_row :: Int -> Int -> Int -> [Complex]
gen_id_row cur one_row n =
  if (cur == one_row)
     then (one : (gen_id_row (cur + 1) one_row n))
     else if (cur >= n)
          then []
          else (zero : (gen_id_row (cur + 1) one_row n))

id_matrix_helper :: Int -> Int -> [[Complex]]
id_matrix_helper cur_row max_rows =
  if (cur_row >= max_rows)
     then []
     else ((gen_id_row 0 cur_row max_rows) : (id_matrix_helper (cur_row + 1) max_rows))

id_matrix :: Int -> [[Complex]]
id_matrix n = id_matrix_helper 0 n


-- main = print((scale_matrix (1,2) [[(1,-1),(3,0)],[(2,2),(4,1)]]))
-- main = print(scale_matrix (0,2) (scale_matrix (1,2) [[(1,-1),(3,0)],[(2,2),(4,1)]]))
-- main = print(scale_matrix (add (0,2) (1,2)) [[(1,-1),(3,0)],[(2,2),(4,1)]])
-- main = print(transpose [[(6,-3),(2,12),(0,-19)],[(0,0),(5,2.1),(17,0)],[(1,0),(2,5),(3,-4.5)]])
-- main = print(adjoint [[(6,-3),(2,12),(0,-19)],[(0,0),(5,2.1),(17,0)],[(1,0),(2,5),(3,-4.5)]])
-- main = print (scale (1.5,0) (scale (2,0) [(0,0), (1,1)]))
-- main = print (scale (3,0) [(0,0), (1,1)])
-- main = print(get_column [[(6,-3),(2,12),(0,-19)],[(0,0),(5,2.1),(17,0)],[(1,0),(2,5),(3,-4.5)]] 1)
-- main = print (matrix_mult [[(3,2),(0,0),(5,-6)],[(1,0),(4,2),(0,1)],[(4,-1),(0,0),(4,0)]] [[(5,0),(2,-1),(6,-4)],[(0,0),(4,5),(2,0)],[(7,-4),(2,7),(0,0)]])
-- main = print (id_matrix 5)
