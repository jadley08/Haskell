{-# LANGUAGE MultiWayIf #-}

--module Heap
  --(Heap,elements,element,isHeap,insert,merge) where

import Data.Maybe

owise :: Bool
owise = otherwise

---------------------------------------------------------
type Cmp v  = v -> v -> Bool
data Heap v = Empty | Heap (Cmp v) v (Heap v) (Heap v)
---------------------------------------------------------

elements :: (Heap v) -> [v]
elements Empty          = []
elements (Heap _ e l r) = e:((elements l) ++ (elements r))

element :: (Eq v) => (Heap v) -> v -> Bool
element Empty v          = False
element (Heap _ e l r) v = if (v == e)
                              then True
                              else (element l v) && (element r v)

size :: (Heap v) -> Integer
size Empty          = 0
size (Heap _ _ l r) = 1 + (size l) + (size r)

peek :: (Heap v) -> Maybe v
peek Empty          = Nothing
peek (Heap _ e _ _) = Just e

isHeap :: (Heap v) -> Bool
isHeap Empty          = True
isHeap (Heap f e l r) = let l_e = peek l
                            r_e = peek r
                         in (isNothing l_e ||
                             (f e (fromJust l_e)))
                            &&
                            (isNothing r_e ||
                             (f e (fromJust r_e)))

insert :: (Heap v) -> v -> (v -> v -> Bool) -> (Heap v)
insert Empty v f          = (Heap f v Empty Empty)
insert (Heap f e l r) v _ =
  let l_c    = size l
      r_c    = size r
      cmp_ve = (f v e)
   in if | cmp_ve && (l_c < r_c) -> (Heap f v (insert l e f) r)
         | cmp_ve                -> (Heap f v l (insert r e f))
         | l_c < r_c             -> (Heap f e (insert l v f) r)
         | owise                 -> (Heap f e l (insert r v f))

merge :: (Heap v) -> (Heap v) -> (v -> v -> Bool) -> (Heap v)
merge h1 Empty _          = h1
merge Empty h2 _          = h2
merge h1 (Heap _ e l r) f = merge (merge (insert h1 e f) l f) r f

fromList :: [v] -> (v -> v -> Bool) -> (Heap v)
fromList []     _ = Empty
fromList [v]    f = Heap f v Empty Empty
fromList (v:vs) f = insert (fromList vs f) v f

---------------------------------------------------------
-------------------------TESTING-------------------------
---------------------------------------------------------

type T = (String,Integer)

f :: T -> T -> Bool
f (_,n) (_,m) = n < m

t0 :: Bool
t0 = size (Heap f ("0",0) (Heap f ("1",1) Empty Empty) Empty) == 2

h1 :: Heap T
h1 = Heap f ("h1",1) Empty Empty
h2 :: Heap T
h2 = insert h1 ("h2",2) f
h3 :: Heap T
h3 = insert h2 ("h3",-1) f
h4 :: Heap T
h4 = insert h3 ("h4",5) f
h5 :: Heap T
h5 = insert h4 ("h5",-2) f
h6 :: Heap T
h6 = insert h5 ("h5",12) f
t1 :: Bool
t1 = isHeap h6
t2 :: Bool
t2 = size h6 == 6

h10 :: Heap T
h10 = fromList [("h10",1),("h11",2),("h12",5),("h13",-24),("h14",0),("h15",-5),("h16",8),("h17",-3),("h18",7),("h19",-24)] f
t3 :: Bool
t3 = isHeap h10
t4 :: Bool
t4 = size h10 == 10

h20 :: Heap T
h20 = merge h10 h6 f

--main = print (size h6)
main = print(t0 && t1 && t2)
