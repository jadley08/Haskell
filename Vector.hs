data Vector a =  Vector { x :: a
                        , y :: a
                        , z :: a
                        } deriving (Show, Eq)


addVector :: (RealFloat t) => Vector t -> Vector t -> Vector t
(Vector i j k) `addVector` (Vector l m n) = Vector (i+l) (j+m) (k+n)


scaleVector :: (RealFloat t) => Vector t -> t -> Vector t
v `scaleVector` n = Vector ((x v)*n) ((y v)*n) ((z v)*n)


getVector :: (RealFloat t) => t -> t -> t -> t -> t -> t -> Vector t
getVector x1 y1 z1 x2 y2 z2 = Vector (x2-x1) (y2-y1) (z2-z1)


lengthVector :: (RealFloat t) => Vector t -> t
lengthVector v = sqrt (((x v) ^ 2) + ((y v) ^ 2) + ((z v) ^ 2))


makeLengthVector :: (RealFloat t) => Vector t -> t -> Vector t
v `makeLengthVector` n = let s = n / (lengthVector v)
                     in (Vector (s*(x v)) (s*(y v)) (s*(z v)))


dotVector :: (RealFloat t) => Vector t -> Vector t -> t
v1 `dotVector` v2 = ((x v1) * (x v2)) + ((y v1) * (y v2)) + ((z v1) * (z v2))


crossVector :: (RealFloat t) => Vector t -> Vector t -> Vector t
v1 `crossVector` v2 = Vector (((y v1) * (z v2)) - ((z v1) * (y v2)))
                             (((z v1) * (x v2)) - ((x v1) * (z v2)))
                             (((x v1) * (y v2)) - ((y v1) * (x v2)))