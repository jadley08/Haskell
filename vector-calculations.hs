-- addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

lengthVector :: (RealFloat a) => (a, a, a) -> a
lengthVector (x, y, z) = sqrt ((x ^ 2) + (y ^ 2) + (z ^ 2))

getVector :: (RealFloat a) => (a, a, a) -> (a, a, a) -> (a, a, a)
getVector (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)

addVectors :: (RealFloat a) => (a, a, a) -> (a, a, a) -> (a, a, a)
addVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

makeVectorLength :: (RealFloat a) => (a, a, a) -> (a) -> (a, a, a)
makeVectorLength (x, y, z) (l) = let scalar  = l `div` (lengthVector (x, y, z))
				 in  ((scalar * x), (scalar * y), (scalar * z))
dotVectors :: (RealFloat a) => (a, a, a) -> (a, a, a) -> a
dotVectors (x1, y1, z1) (x2, y2, z2) = ((x1 * x2) + (y1 * y2) + (z1 * z2))

crossVectors :: (RealFloat a) => (a, a, a) -> (a, a, a) -> (a, a, a)
crossVectors (x1, y1, z1) (x2, y2, z2) = (((y1 * z2) - (z1 * y2)), ((z1 * x2) - (x1 * z2)), ((x1* y2) - (y1 * x2)))
