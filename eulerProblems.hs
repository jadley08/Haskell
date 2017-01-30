import Data.Char as Char

-----------------------------------------------------------------------------------
--add up all multiples of 3 and 5
problem1 :: (Integral a) => [a] -> a
problem1 [] = 0
problem1 (x:xs)
  | (x `mod` 3) == 0 || (x `mod` 5) == 0 = x + problem1 xs
  | otherwise = problem1 xs
--problem1 [1..99] -- 2318 -- 233168

-----------------------------------------------------------------------------------
--add up even fibbonacci numbers
problem2 :: (Integral a) => [a] -> a
problem2 xs = let num = (last xs) + (last (init xs))
              in  if num >= 4000000
                 then sum [x | x <- xs, x `mod` 2 == 0]
                 else problem2 (xs ++ [num])
             --where num = (last xs) + (last (init xs))
--problem2 [1, 2] -- 4613732

-------------------------------------------------------------------------------
--smallest positive number evenly divisible by [1..20]
--problem5 :: (Integral a) => [a] -> a
--problem5 (x:xs) = if (foldl (\acc (y:ys) -> ((x `mod` y) && acc)) True [1..20]) == True
--                  then x
--                  else problem5 xs
-- problem5 [1..]

problem5' :: (Integral a) => [a] -> [a] -> a
problem5' [] ys = error "Could not find a number"
problem5' (x:xs) ys
  | (divisible' x ys) == True = x
  | otherwise = problem5' xs ys
--problem5' [1..] [1..20] -- 232792560
--problem5' [1..] [1..10] -- 2520

divisible' :: (Integral a) => a -> [a] -> Bool
divisible' z [] = True
divisible' z (x:xs) = if (z `mod` x) == 0
                      then divisible' z xs
                      else False

--------------------------------------------------------------------------------
problem6 :: (Integral a) => [a] -> a -> a -> a
problem6 [] y z = (z * z) - y
problem6 (x:xs) y z = problem6 xs (y + (x * x)) (z + x)
--problem6 [1..100] 0 0 -- 25164150

--------------------------------------------------------------------------------
problem7 :: (Num t, Eq t) => [Int] -> t -> Int -> Int
problem7 _ 10002 z = z
problem7 (x:xs) y q
  | divisible'' x [2..(isqrt x)] = problem7 xs (y + 1) x
  | otherwise = problem7 xs y q
--problem7 [1..] 0 0 -- 104743

divisible'' :: (Integral a) => a -> [a] -> Bool
divisible'' _ [] = True
divisible'' y (x:xs) = if (y `mod` x) == 0
                       then False
                       else divisible'' y xs

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

----------------------------------------------------------------------------------
--problem8 :: (Integral a) -> String -> a -> a
--problem8 [] q = q
--problem8 xs q = let p = foldl (\acc x -> acc * (Char.digitToInt x)) 1 (take 13 xs)
--                in if (p > q)
--                   then problem8 (drop 13 xs) p
--                   else problem8 (drop 13 xs) q

--problem8' "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450" 0

problem8' :: [Char] -> Int -> Int
problem8' [] q = q
problem8' str q = let p = (takeMult (take 13 str) 1)
                  in if (p > q)
                     then problem8' (drop 13 str) p
                     else problem8' (drop 13 str) q
--6270566400

takeMult :: [Char] -> Int -> Int
takeMult [] p = p
takeMult (x:xs) p = takeMult xs (p * (Char.digitToInt x))
--takeMult "............." 1 = .*.*.*.*.*.*.*.*.*.*.*.*.

------------------------------------------------------------------------------
--largest product of 2 3-digit numbers that are palindromes
problem4helper :: (Show t, Ord t, Num t) => [t] -> t -> t -> t
problem4helper [] _ z = z
problem4helper (x:xs) y z = if (isPalindrome x y)
                            then if ((x * y) > z)
                                 then problem4helper xs y (x * y)
                                 else problem4helper xs y z
                            else problem4helper xs y z
                            
problem4 :: (Show t, Ord t, Num t) => [t] -> [t] -> t -> t
problem4 _ [] z = z
problem4 x (y:ys) z = let p = problem4helper x y z
                      in if (p > z)
                         then problem4 x ys p
                         else problem4 x ys z
--problem4 [999,998..100] [999,998..100] 0 - 906609

isPalindrome :: (Show t, Num t) => t -> t -> Bool
isPalindrome x y = let n = (show (x * y))
                   in if (length n == 5)
                      then (((head n) == (last n)) && ((head $ tail n) == (last $ init n)))
                      else (((head n) == (last n)) && ((head $ tail n) == (last $ init n)) && ((head $ tail $ tail n) == (last $ init $ init n)))

-----------------------------------------------------------------------------------
problem13 :: (Show t, Num t) => [t] -> t -> [Char]
problem13 [] z = take 10 $ show z
problem13 (x:xs) z = problem13 xs (z + x)
--problem13 [..] 0 - "5537376230"