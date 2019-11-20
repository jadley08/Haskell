{-# LANGUAGE RankNTypes #-}

type Nat = Integer
type Church = forall x. (x -> x) -> x -> x

suc :: Nat -> Nat
suc x = x + 1

zro :: Nat
zro = 0

-----------------------------------

czero :: Church
czero = (\f b -> b)

cone :: Church
cone = (\f b -> f b)

ctwo :: Church
ctwo = (\f b -> f (f b))

cthree :: Church
cthree = (\f b -> f (f (f b)))

-----------------------------------

csuc :: Church -> Church
csuc n = (\f b -> f (n f b))

cplus :: Church -> Church -> Church
cplus n m = (\f b -> m f (n f b))

cmult :: Church -> Church -> Church
cmult n m = (\f b -> m (n f) b)

cexp :: Church -> Church -> Church
cexp n m = m n

-----------------------------------
t1 :: Bool
t1 = czero suc zro == 0
t2 :: Bool
t2 = cone suc zro == 1
t3 :: Bool
t3 = ctwo suc zro == 2
t4 :: Bool
t4 = cthree suc zro == 3
t5 :: Bool
t5 = csuc cone suc zro == ctwo suc zro
t6 :: Bool
t6 = csuc cone suc zro == ctwo suc zro
t7 :: Bool
t7 = cplus cone cone suc zro == ctwo suc zro
t8 :: Bool
t8 = cplus czero cthree suc zro == cthree suc zro
t9 :: Bool
t9 = cplus ctwo cone suc zro == cthree suc zro
t10 :: Bool
t10 = cplus czero czero suc zro == czero suc zro
t11 :: Bool
t11 = cmult czero cthree suc zro == czero suc zro
t12 :: Bool
t12 = cmult cthree czero suc zro == czero suc zro
t13 :: Bool
t13 = cmult cthree ctwo suc zro == 6
t14 :: Bool
t14 = cexp ctwo cthree suc zro == 8
t15 :: Bool
t15 = cexp cthree ctwo suc zro == 9
t16 :: Bool
t16 = cexp czero cthree suc zro == czero suc zro
t17 :: Bool
t17 = cexp cthree czero suc zro == cone suc zro

main = print(t1 && t2 && t3 && t4 && t5 && t6 && t7 && t8 && t9 && t10 && t11 && t12 && t13 && t14 && t15 && t16 && t17)
