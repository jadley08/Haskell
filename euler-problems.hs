import qualified Data.Map as Map

-- Problem 1: add up all multiples of 3 and 5 below 1000
-- result: 25164150

problem1Helper :: [Integer] -> Integer
problem1Helper [] = 0
problem1Helper (x:xs)
  | (x `mod` 3) == 0 || (x `mod` 5) == 0 = x + problem1Helper xs
  | otherwise = problem1Helper xs

problem1 :: Integer
problem1 = problem1Helper [1..999]


-----------------------------------------------------------------------
-- Problem 2: Sum even fibonacci numbers who do not exceed four million
-- result: 233168

problem2Helper :: Integer -> Integer -> Integer -> Integer
problem2Helper a b acc
  | (a + b) > 4000000 = acc
  | otherwise = problem2Helper b (a + b) (acc + a + b)

problem2 :: Integer
problem2 = problem2Helper 0 1 0


-----------------------------------------------------------------------
-- Problem 3: Largest prime factor of 600851475143
-- result: 6857

problem3Helper :: Integer -> Integer -> Integer
problem3Helper x curDivisor
  | (x <= curDivisor) = curDivisor
  | ((x `mod` curDivisor) == 0) = problem3Helper (x `quot` curDivisor) 2
  | otherwise = problem3Helper x (curDivisor + 1)

problem3 :: Integer
problem3 = problem3Helper 600851475143 2


----------------------------------------------------------------------
-- Problem 4: Largest palindrome made from the product of two 3-digit numbers
-- result: 580085

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
  go :: [a] -> [a] -> [a]
  go [] acc = acc
  go (x:xs) acc = go xs (x:acc)

isPalindrome :: Eq a => [a] -> Integer
isPalindrome s
  | s == (reverse s) = 1
  | otherwise = 0

problem4Helper :: Integer -> Integer -> Integer -> Integer
problem4Helper x y acc
  | y >= 1000 = acc
  | x >= 1000 = problem4Helper 100 (y + 1) acc
  | (isPalindrome (show (x * y))) == 1 = problem4Helper (x + 1) y (x * y)
  | otherwise = problem4Helper (x + 1) y acc

problem4 :: Integer
problem4 = problem4Helper 100 100 0


---------------------------------------------------------------------
-- Problem5: Smallest positive number evenly divisible by all numbers from 1 to 20
-- result: 232792560

isDivByAll :: [Integer] -> Integer -> Integer
isDivByAll [] _ = 1
isDivByAll (x:xs) y
  | (y `mod` x) == 0 = isDivByAll xs y
  | otherwise = 0

problem5Helper :: [Integer] -> Integer -> Integer
problem5Helper ls x
  | (isDivByAll ls x) == 1 = x
  | otherwise = problem5Helper ls (x + 1)

problem5 :: Integer
problem5 = problem5Helper [1..20] 1

--------------------------------------------------------------------
-- Problem 6: Difference between sum of squares of first 100 nats and the square of the sum of the first 100 nats
-- result: 25164150

problem6Helper :: [Integer] -> [Integer] -> Integer -> Integer -> Integer
problem6Helper [] [] a b
  | (b * b) > a = (b * b) - a
  | otherwise = a - (b * b)
problem6Helper [] _ _ _ = 0
problem6Helper _ [] _ _ = 0
problem6Helper (x:xs) (y:ys) a b = problem6Helper xs ys ((x * x) + a) (y + b)

problem6 :: Integer
problem6 = problem6Helper [1..100] [1..100] 0 0

--------------------------------------------------------------------
-- Problem 7: 10001st prime number?
-- result: 104743

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..(k - 1)], k `mod` x == 0]

problem7 :: Integer
problem7 = (filter isPrime [2..])!!10000


--------------------------------------------------------------------
-- Problem 8: Thirteen adjacent digits that have greatest product, find the value
-- result: 64497254400

ls :: [[Int]]
ls =   [[7,3,1,6,7,1,7,6,5,3,1,3,3,0,6,2,4,9,1,9,2,2,5,1,1,9,6,7,4,4,2,6,5,7,4,7,4,2,3,5,5,3,4,9,1,9,4,9,3,4],
        [9,6,9,8,3,5,2,0,3,1,2,7,7,4,5,0,6,3,2,6,2,3,9,5,7,8,3,1,8,0,1,6,9,8,4,8,0,1,8,6,9,4,7,8,8,5,1,8,4,3],
        [8,5,8,6,1,5,6,0,7,8,9,1,1,2,9,4,9,4,9,5,4,5,9,5,0,1,7,3,7,9,5,8,3,3,1,9,5,2,8,5,3,2,0,8,8,0,5,5,1,1],
        [1,2,5,4,0,6,9,8,7,4,7,1,5,8,5,2,3,8,6,3,0,5,0,7,1,5,6,9,3,2,9,0,9,6,3,2,9,5,2,2,7,4,4,3,0,4,3,5,5,7],
        [6,6,8,9,6,6,4,8,9,5,0,4,4,5,2,4,4,5,2,3,1,6,1,7,3,1,8,5,6,4,0,3,0,9,8,7,1,1,1,2,1,7,2,2,3,8,3,1,1,3],
        [6,2,2,2,9,8,9,3,4,2,3,3,8,0,3,0,8,1,3,5,3,3,6,2,7,6,6,1,4,2,8,2,8,0,6,4,4,4,4,8,6,6,4,5,2,3,8,7,4,9],
        [3,0,3,5,8,9,0,7,2,9,6,2,9,0,4,9,1,5,6,0,4,4,0,7,7,2,3,9,0,7,1,3,8,1,0,5,1,5,8,5,9,3,0,7,9,6,0,8,6,6],
        [7,0,1,7,2,4,2,7,1,2,1,8,8,3,9,9,8,7,9,7,9,0,8,7,9,2,2,7,4,9,2,1,9,0,1,6,9,9,7,2,0,8,8,8,0,9,3,7,7,6],
        [6,5,7,2,7,3,3,3,0,0,1,0,5,3,3,6,7,8,8,1,2,2,0,2,3,5,4,2,1,8,0,9,7,5,1,2,5,4,5,4,0,5,9,4,7,5,2,2,4,3],
        [5,2,5,8,4,9,0,7,7,1,1,6,7,0,5,5,6,0,1,3,6,0,4,8,3,9,5,8,6,4,4,6,7,0,6,3,2,4,4,1,5,7,2,2,1,5,5,3,9,7],
        [5,3,6,9,7,8,1,7,9,7,7,8,4,6,1,7,4,0,6,4,9,5,5,1,4,9,2,9,0,8,6,2,5,6,9,3,2,1,9,7,8,4,6,8,6,2,2,4,8,2],
        [8,3,9,7,2,2,4,1,3,7,5,6,5,7,0,5,6,0,5,7,4,9,0,2,6,1,4,0,7,9,7,2,9,6,8,6,5,2,4,1,4,5,3,5,1,0,0,4,7,4],
        [8,2,1,6,6,3,7,0,4,8,4,4,0,3,1,9,9,8,9,0,0,0,8,8,9,5,2,4,3,4,5,0,6,5,8,5,4,1,2,2,7,5,8,8,6,6,6,8,8,1],
        [1,6,4,2,7,1,7,1,4,7,9,9,2,4,4,4,2,9,2,8,2,3,0,8,6,3,4,6,5,6,7,4,8,1,3,9,1,9,1,2,3,1,6,2,8,2,4,5,8,6],
        [1,7,8,6,6,4,5,8,3,5,9,1,2,4,5,6,6,5,2,9,4,7,6,5,4,5,6,8,2,8,4,8,9,1,2,8,8,3,1,4,2,6,0,7,6,9,0,0,4,2],
        [2,4,2,1,9,0,2,2,6,7,1,0,5,5,6,2,6,3,2,1,1,1,1,1,0,9,3,7,0,5,4,4,2,1,7,5,0,6,9,4,1,6,5,8,9,6,0,4,0,8],
        [0,7,1,9,8,4,0,3,8,5,0,9,6,2,4,5,5,4,4,4,3,6,2,9,8,1,2,3,0,9,8,7,8,7,9,9,2,7,2,4,4,2,8,4,9,0,9,1,8,8],
        [8,4,5,8,0,1,5,6,1,6,6,0,9,7,9,1,9,1,3,3,8,7,5,4,9,9,2,0,0,5,2,4,0,6,3,6,8,9,9,1,2,5,6,0,7,1,7,6,0,6],
        [0,5,8,8,6,1,1,6,4,6,7,1,0,9,4,0,5,0,7,7,5,4,1,0,0,2,2,5,6,9,8,3,1,5,5,2,0,0,0,5,5,9,3,5,7,2,9,7,2,5],
        [7,1,6,3,6,2,6,9,5,6,1,8,8,2,6,7,0,4,2,8,2,5,2,4,8,3,6,0,0,8,2,3,2,5,7,5,3,0,4,2,0,7,5,2,9,6,3,4,5,0]]

multThirteen :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int
multThirteen ls x y modX modY count
  | count == 13 = 1
  | x < 0 || y < 0 || x > 49 || y > 19 = 0
  | otherwise = ls!!y!!x * (multThirteen ls (x + modX) (y + modY) modX modY (count + 1))

problem8Helper :: [[Int]] -> Int -> Int -> Int
problem8Helper ls x y
  | y > 19 = 0
  | x > 49 = problem8Helper ls 0 (y + 1)
  | otherwise = (max (multThirteen ls x y 0 (0 - 1) 0)
                     (max (multThirteen ls x y 1 (0 - 1) 0)
                          (max (multThirteen ls x y 1 0 0)
                               (max (multThirteen ls x y 1 1 0)
                                    (problem8Helper ls (x + 1) y)))))


problem8 :: Int
problem8 = problem8Helper ls 0 0


--------------------------------------------------------------------
-- Problem 9: find a*b*c where a+b+c=1000 and a,b,c is a pythagorean triplet
-- pythagorean triplet: a<b<c :: a^2+b^2=c^2
-- result: out of memory

problem9Helper :: Int -> Int -> Int -> Int
problem9Helper a b c
  | c > 1000 = problem9Helper a (b + 1) 0
  | b > 1000 = problem9Helper (a + 1) b c
  | a > 1000 = (0 - 1)
  | a >= b || b >= c || (a + b + c) /= 1000 || ((a * a) + (b * b)) /= (c * c) = problem9Helper a b (c + 1)
  | otherwise = a * b * c

problem9 :: Int
problem9 = problem9Helper 0 0 0


--------------------------------------------------------------------
-- Problem 10: Sum of all primes below 2000000
-- result: gave up waiting

sumAcc :: [Integer] -> Integer -> Integer
sumAcc [] acc = acc
sumAcc (x:xs) acc = sumAcc xs (x + acc)

problem10 :: Integer
problem10 = sumAcc (filter isPrime [2..2000000]) 0


--------------------------------------------------------------------
-- Problem 11: Greatest product of four adjacent numbers
-- result: 70600674

ls2 :: [[Int]]
ls2 = [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
       [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
       [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
       [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
       [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
       [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
       [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
       [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
       [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
       [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
       [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
       [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
       [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
       [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
       [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
       [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
       [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
       [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
       [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
       [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

multFour :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int
multFour ls x y modX modY count
  | count == 4 = 1
  | x < 0 || y < 0 || x > 19 || y > 19 = 0
  | otherwise = ls!!y!!x * (multFour ls (x + modX) (y + modY) modX modY (count + 1))

problem11Helper :: [[Int]] -> Int -> Int -> Int
problem11Helper ls x y
  | y > 19 = 0
  | x > 19 = problem11Helper ls 0 (y + 1)
  | otherwise = (max (multFour ls x y 0 (0 - 1) 0)
                     (max (multFour ls x y 1 (0 - 1) 0)
                          (max (multFour ls x y 1 0 0)
                               (max (multFour ls x y 1 1 0)
                                    (problem11Helper ls (x + 1) y)))))


problem11 :: Int
problem11 = problem11Helper ls2 0 0


--------------------------------------------------------------------
-- Problem 12: First triangle number (Σ) to have over five hundred divisors
-- result: 

getCountDivisors :: Int -> [Int] -> Int -> Int
getCountDivisors _ [] acc = acc
getCountDivisors 0 _ acc = acc
getCountDivisors num (x:xs) acc
  | (num `mod` x) == 0 = getCountDivisors num xs (acc + 1)
  | otherwise = getCountDivisors num xs acc

problem12Helper :: Int -> Int -> Int
problem12Helper index lastTriNum
  | getCountDivisors lastTriNum [1..lastTriNum] 0 > 500 = lastTriNum
  | otherwise = problem12Helper (index + 1) (lastTriNum + index)

problem12 :: Int
problem12 = problem12Helper 0 0


--------------------------------------------------------------------
-- Problem 13: Find first ten digits of sum
-- result: 7061221098
-- commented out due to haskell warnings
{-
sumBigNums :: Int
sumBigNums = 37107287533902102798797998220837590246510135740250 +
             46376937677490009712648124896970078050417018260538 +
             74324986199524741059474233309513058123726617309629 +
             91942213363574161572522430563301811072406154908250 +
             23067588207539346171171980310421047513778063246676 +
             89261670696623633820136378418383684178734361726757 +
             28112879812849979408065481931592621691275889832738 +
             44274228917432520321923589422876796487670272189318 +
             47451445736001306439091167216856844588711603153276 +
             70386486105843025439939619828917593665686757934951 +
             62176457141856560629502157223196586755079324193331 +
             64906352462741904929101432445813822663347944758178 +
             92575867718337217661963751590579239728245598838407 +
             58203565325359399008402633568948830189458628227828 +
             80181199384826282014278194139940567587151170094390 +
             35398664372827112653829987240784473053190104293586 +
             86515506006295864861532075273371959191420517255829 +
             71693888707715466499115593487603532921714970056938 +
             54370070576826684624621495650076471787294438377604 +
             53282654108756828443191190634694037855217779295145 +
             36123272525000296071075082563815656710885258350721 +
             45876576172410976447339110607218265236877223636045 +
             17423706905851860660448207621209813287860733969412 +
             81142660418086830619328460811191061556940512689692 +
             51934325451728388641918047049293215058642563049483 +
             62467221648435076201727918039944693004732956340691 +
             15732444386908125794514089057706229429197107928209 +
             55037687525678773091862540744969844508330393682126 +
             18336384825330154686196124348767681297534375946515 +
             80386287592878490201521685554828717201219257766954 +
             78182833757993103614740356856449095527097864797581 +
             16726320100436897842553539920931837441497806860984 +
             48403098129077791799088218795327364475675590848030 +
             87086987551392711854517078544161852424320693150332 +
             59959406895756536782107074926966537676326235447210 +
             69793950679652694742597709739166693763042633987085 +
             41052684708299085211399427365734116182760315001271 +
             65378607361501080857009149939512557028198746004375 +
             35829035317434717326932123578154982629742552737307 +
             94953759765105305946966067683156574377167401875275 +
             88902802571733229619176668713819931811048770190271 +
             25267680276078003013678680992525463401061632866526 +
             36270218540497705585629946580636237993140746255962 +
             24074486908231174977792365466257246923322810917141 +
             91430288197103288597806669760892938638285025333403 +
             34413065578016127815921815005561868836468420090470 +
             23053081172816430487623791969842487255036638784583 +
             11487696932154902810424020138335124462181441773470 +
             63783299490636259666498587618221225225512486764533 +
             67720186971698544312419572409913959008952310058822 +
             95548255300263520781532296796249481641953868218774 +
             76085327132285723110424803456124867697064507995236 +
             37774242535411291684276865538926205024910326572967 +
             23701913275725675285653248258265463092207058596522 +
             29798860272258331913126375147341994889534765745501 +
             18495701454879288984856827726077713721403798879715 +
             38298203783031473527721580348144513491373226651381 +
             34829543829199918180278916522431027392251122869539 +
             40957953066405232632538044100059654939159879593635 +
             29746152185502371307642255121183693803580388584903 +
             41698116222072977186158236678424689157993532961922 +
             62467957194401269043877107275048102390895523597457 +
             23189706772547915061505504953922979530901129967519 +
             86188088225875314529584099251203829009407770775672 +
             11306739708304724483816533873502340845647058077308 +
             82959174767140363198008187129011875491310547126581 +
             97623331044818386269515456334926366572897563400500 +
             42846280183517070527831839425882145521227251250327 +
             55121603546981200581762165212827652751691296897789 +
             32238195734329339946437501907836945765883352399886 +
             75506164965184775180738168837861091527357929701337 +
             62177842752192623401942399639168044983993173312731 +
             32924185707147349566916674687634660915035914677504 +
             99518671430235219628894890102423325116913619626622 +
             73267460800591547471830798392868535206946944540724 +
             76841822524674417161514036427982273348055556214818 +
             97142617910342598647204516893989422179826088076852 +
             87783646182799346313767754307809363333018982642090 +
             10848802521674670883215120185883543223812876952786 +
             71329612474782464538636993009049310363619763878039 +
             62184073572399794223406235393808339651327408011116 +
             66627891981488087797941876876144230030984490851411 +
             60661826293682836764744779239180335110989069790714 +
             85786944089552990653640447425576083659976645795096 +
             66024396409905389607120198219976047599490197230297 +
             64913982680032973156037120041377903785566085089252 +
             16730939319872750275468906903707539413042652315011 +
             94809377245048795150954100921645863754710598436791 +
             78639167021187492431995700641917969777599028300699 +
             15368713711936614952811305876380278410754449733078 +
             40789923115535562561142322423255033685442488917353 +
             44889911501440648020369068063960672322193204149535 +
             41503128880339536053299340368006977710650566631954 +
             81234880673210146739058568557934581403627822703280 +
             82616570773948327592232845941706525094512325230608 +
             22918802058777319719839450180888072429661980811197 +
             77158542502016545090413245809786882778948721859617 +
             72107838435069186155435662884062257473692284509516 +
             20849603980134001723930671666823555245252804609722 +
             53503534226472524250874054075591789781264330331690

problem13 :: [Char]
problem13 = take 10 (show sumBigNums)
-}


--------------------------------------------------------------------
-- Problem 14: longest sequence to finish at 1 start term from 1 to 1000000
-- n -> n/2  (n even)
-- n -> 3n+1 (n odd)
--inneficient, should redo it (fill store more efficiently)

getCollatzLength :: Int -> (Map.Map Int Int) -> Int
getCollatzLength x store =
  case (Map.lookup x store) of
    Nothing -> 1 + if (even x)
                   then getCollatzLength (x `quot` 2) store --should be adding these to the store, redo this whole problem
                   else getCollatzLength ((3 * x) + 1) store
    Just c -> c

problem14Helper2 :: Int -> (Map.Map Int Int) -> Int
problem14Helper2 x store
  | x == 1000001 = snd (Map.findMax store)
  | otherwise = problem14Helper2 (x + 1) (Map.insert x (getCollatzLength x store) store)

problem14 :: Int
problem14 = problem14Helper2 0 Map.empty


--------------------------------------------------------------------
-- Problem 15: only able to more left and right, how many ways to get through 20x20 grid

problem15Helper :: Int -> Int -> Int
problem15Helper 20 20 = 0
problem15Helper 20 _ = 0
problem15Helper _ 20 = 0
problem15Helper x y = 2 + (problem15Helper (x + 1) y) + (problem15Helper x (y + 1))

problem15 :: Int
problem15 = problem15Helper 0 0


--------------------------------------------------------------------
-- Problem 16: sum of digits of number 2^1000
-- incomplete
p16_num :: [Int]
p16_num = replicate 302 0

-- index starts from 1
p16_replace :: Int -> Int -> [Int] -> [Int]
p16_replace 0 val [] = []
p16_replace 0 val (x:xs) = val : xs
p16_replace index val [] = []
p16_replace index val (x:xs) = x : (p16_replace (index - 1) val xs)

mult_by_two :: [Int] -> [Int]
mult_by_two ls = ls --here

problem16Helper :: Int -> [Int] -> [Int]
problem16Helper 1001 ls = ls
problem16Helper x ls = problem16Helper (x + 1) (mult_by_two ls)

problem16 :: Int
problem16 = foldl (+) 0 (problem16Helper 1 (p16_replace 0 2 ls))
