{-# LANGUAGE MultiWayIf #-}
--import Data.List
--import Data.Bool
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

insert :: (Heap v) -> v -> (Heap v)
insert Empty v          = Empty
insert (Heap f e l r) v =
  let l_c    = size l
      r_c    = size r
      cmp_ve = (f v e)
   in if | cmp_ve && (l_c < r_c) -> (Heap f v (insert l e) r)
         | cmp_ve                -> (Heap f v l (insert r e))
         | l_c <= r_c            -> (Heap f e (insert l v) r)
         | owise                 -> (Heap f e l (insert r v))

merge :: (Heap v) -> (Heap v) -> (Heap v)
merge h1 Empty          = h1
merge Empty h2          = h2
merge h1 (Heap _ e l r) = merge (merge (insert h1 e) l) r

---------------------------------------------------------
-------------------------TESTING-------------------------
---------------------------------------------------------

main = print()
