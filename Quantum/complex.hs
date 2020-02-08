type Complex = (Float,Float)
type Polar   = (Float,Float)


re :: Complex -> Float
re (x, y) = x

im :: Complex -> Float
im (x, y) = y

add :: Complex -> Complex -> Complex
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

times :: Complex -> Complex -> Complex
times (x1, y1) (x2, y2) = ((x1 * x2) - (y1 * y2), (x1 * y2) + (y1 * x2))

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

main = print(nth_root (to_polar (1,1)) 3)
