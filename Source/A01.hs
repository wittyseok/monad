-- | Assignment 1: implementing various small functions
module A01
  ( Day(..)
  , nextWeekday
  , addTuple
  , productDot
  , maybeMap
  , maybeThen
  , Tree(..)
  , sumTree
  , rightRotateTree
  , listSum
  , productSeq
  , setMem
  , setEquiv
  , setUnion
  , setIntersection
  , setDiff
  , setSymDiff
  , relMem
  , relEquiv
  , relComp
  , relTrans
  , relFull
  , fibs
  , primes
  , fuzzySeq
  , funComp
  , curry2
  , uncurry2
  , myFilter
  , myFilterMap
  , myFoldL
  , myRev
  ) where
import Data.List
-- | TODO marker.
todo :: t
todo = error "todo"

-- | Days of week.
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show)

-- | Returns the next weekday (excluding weekend, namely Saturday and Sunday).
nextWeekday :: Day -> Day
nextWeekday Sunday = Monday
nextWeekday Monday = Tuesday
nextWeekday Tuesday = Wednesday
nextWeekday Wednesday = Thursday
nextWeekday Thursday = Friday
nextWeekday Friday = Monday
nextWeekday Saturday = Monday

-- | Add tuples of the 2-dimensional plane.
addTuple :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addTuple t1 t2 = (fst t1 + fst t2, snd t1 + snd t2)

-- | Dot-products two integer (list) vectors: https://en.wikipedia.org/wiki/Dot_product
-- |
-- | If the two vectors have different number of elements, you can return anything.
productDot :: [Integer] -> [Integer] -> Integer
productDot t1 t2 = if length t1 == length t2 then sum (zipWith (*) t1 t2) else 0

-- | Maps the given value if it's Just.
maybeMap :: (Integer -> Integer) -> Maybe Integer -> Maybe Integer
maybeMap f value = fmap f (value)

-- | If the given value is Just, map it with the given function; otherwise, the result is Nothing.
maybeThen :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
maybeThen value cont = case value of
  Nothing -> Nothing
  Just x -> cont x

-- | Trees of integers.
data Tree = Leaf Integer | Branch Integer Tree Tree deriving (Eq, Show) -- Integer is value, Trees are left/right subtrees.

-- | Sums all the integers in the given tree.
sumTree :: Tree -> Integer
sumTree tree = case tree of
  Leaf x -> x
  Branch y l r -> y + sumTree(l) + sumTree(r)

-- | Right-rotate the given tree. See https://en.wikipedia.org/wiki/Tree_rotation for more detail.
-- |
-- | Returns Nothing if there are not enough nodes.
rightRotateTree :: Tree -> Maybe Tree
rightRotateTree tree = case tree of
  Leaf x -> Nothing
  Branch a (Branch x  y z) b -> Just(Branch x y (Branch a z b))

-- | Maps the given list.
listMap = map

-- | Sums all the integers in the given list.
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = x + sum xs

-- | More compositional construction of sigma.
sumSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumSeq f from to = listSum (listMap f [from .. to])

-- | product of a sequence. See https://en.wikipedia.org/wiki/Multiplication#Product_of_a_sequence for more detail.
productSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
productSeq f from to = product (listMap f [from .. to])

-- | Returns if the given value is in the (list) set.
setMem :: Integer -> [Integer] -> Bool
setMem value set = if value `elem` set then True else False

-- | function that remove duplicate
remove :: Eq a => [a] -> [a]
remove [] = []
remove (x:xs) 
  | x `elem` xs = remove xs 
  | otherwise = x : remove xs

-- | Returns the two sets contain the same elements.
setEquiv :: [Integer] -> [Integer] -> Bool
setEquiv s1 s2 = if sort (remove s1) ==  sort (remove s2) then True else False

-- | Returns the set union.
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion s1 s2 = remove (s1 ++ s2)

-- | Returns the set intersection 
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection s1 s2 = [x| x<-s1, y<-s2 , x==y]

-- | - Returns the set diff, i.e., setDiff a b = $a - b$.
setDiff :: [Integer] -> [Integer] -> [Integer]
setDiff s1 s2 = [x|x<-s1, x`notElem` setIntersection s1 s2]

-- | Returns the set symmetric diff.
setSymDiff :: [Integer] -> [Integer] -> [Integer]
setSymDiff s1 s2 = setDiff (setUnion s1 s2) (setIntersection s1 s2) 

-- | Returns if the given pair is in the (list) relation.
relMem :: [(Integer, Integer)] -> Integer -> Integer -> Bool
relMem rel v1 v2 = if (v1,v2) `elem` rel then True else False

-- | Returns the two relations contain the same elements.
relEquiv :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
relEquiv r1 r2 = if sort (remove r1) ==  sort (remove r2) then True else False

-- | Composes two relations, i.e., {(a,c) | exists b, (a,b) in r1 and (b,c) in r2}.
relComp :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
relComp r1 r2 = [(fst x,snd y)| x<-r1, y<-r2, snd x == fst y]

-- | Returns the transitive closure of the given relation: https://en.wikipedia.org/wiki/Transitive_closure
relTrans :: [(Integer, Integer)] -> [(Integer, Integer)]
relTrans rel
  | rel == remove(rel ++ [(a,c)|(a,b)<-rel,(b',c)<-rel,b==b']) = rel
  | otherwise = relTrans (remove(rel ++ [(a,c)|(a,b)<-rel,(b',c)<-rel,b==b']))

-- | Returns the relation [0..n] * [0..n] = {(0,0), (0,1), ..., (0,n), (1,0), (1,1), ..., (1,n), ..., (n,n)}.
relFull :: Integer -> [(Integer, Integer)]
relFull n = [(x,y)|x<-[0..n],y<-[0..n]]

-- | The Fibonacci sequence, starting with 0, 1, 1, 2, 3, ...
fibs :: [Integer]
fibs = [0,1] ++ [fibs !!(n-1) + fibs !!(n-2)| n<-[2 ..] ]

-- | The primes, starting with 2, 3, 5, 7, ... 
primes :: [Integer] 
primes = [x| x<-[2 ..] , determine_prime x ]
  where determine_prime n = (([p|p <-[1 .. n], mod n p ==0]) == [1,n])

-- | The sequence of 1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1, ...
fuzzySeq :: [Integer]
fuzzySeq = [fst t - snd t|t<-[(i,j)|i<-[2..],j<-[1..i-1]],fst t > snd t]

-- | Composes two functions, i.e., applies f1 and then f2 to the given argument
funComp :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer)
funComp f1 f2 = fmap f2 f1

-- | Transforms a function that gets single pair into a function that gets two arguments, i.e., curry2 f a1 a2 = f (a1, a2)
curry2 :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Integer
curry2 f a1 a2 = f (a1,a2)

-- | Transforms a function that gets two arguments into a function that gets single pair, i.e., uncurry2 f (a1, a2) = f a1 a2
uncurry2 :: (Integer -> Integer -> Integer) -> (Integer, Integer) -> Integer
uncurry2 f (a1, a2) = a1 `f` a2

-- | Filters the given list so that the the filter function returns True for the remaining elements.
myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter f l = filter(f) l


-- | Maps the given list. If the map function returns Nothing, just drop it.
myFilterMap :: (Integer -> Maybe Integer) -> [Integer] -> [Integer]
myFilterMap f l =[ removeMaybe(f x) |x<-l, (f x) /= Nothing]
  where removeMaybe (Just x) = x

-- | Folds the list from the left, i.e., myFoldL init f [l1, l2, ..., ln] = (f (f (f (f init l1) l2) ...) ln).
myFoldL :: Integer -> (Integer -> Integer -> Integer) -> [Integer] -> Integer
myFoldL init f l = foldl f init l
  

-- | Reverses the given list.
myRev :: [Integer] -> [Integer]
myRev l = reverse(l)