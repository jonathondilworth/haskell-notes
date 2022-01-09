### Lists

*<small>[Intro To Lists Here](https://github.com/jonathondilworth/detailed-plutus-lecture-notes/blob/main/haskell-notes/2-Lecture-Notes-Recursion-And-Lists.md)</small>*

#### Lists: Example Code

```haskell
data [a] = []
         | a : [a]
         -- deriving (...)
         
-- Constructors
[]   :: [a]
(:)  :: a -> [a] -> [a]

-- Functions On Lists
Prelude> :{
Prelude| funcLS :: Num a => [a] -> a
Prelude| funcLS [] = 0
Prelude| funcLS (x:xs) = x + funcLS xs
Prelude| :}
Prelude> funcLS [1,7,5,4,99]
116
Prelude> funcLS [4,36,2]
42
```

#### Lists: Food For Thought

*Basic Functions On Lists: Summation*

It's important to note: the identity of (+) is zero. Thus, our terminating recursive case (the empty list) is zero. Further, lets eval the formal param (list of Ints) in two ways whilst both are recursive, does the eval order make any difference in this case? Why? See later examples for summation with foldl and foldr. When using foldl and foldr does it matter which is used for summation? Why?

How about other functions, like... division (doubles, we're using Num => a) or... can you think of other functions where the slightest change in the implementation of the recursive call would make a difference?

<details>

<summary>Note Regarding The Above Questions</summary>

*Okay, so this is 'food for thought' - what I am essentially getting at is: the order of evaluation matters depending on the operation when using functions such as folds.*

</details>

```haskell
funcLS [1,2,3,4,5,6,7]
28
funcRS [1,2,3,4,5,6,7]
28
-- okay, premature optimisation is the root of all evil, right?
-- but, which do you think is the most efficient?
:i foldl
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  ...
  foldl :: (b -> a -> b) -> b -> t a -> b
  ...
    -- Defined in ‘Data.Foldable’
:i foldr
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  ...
  foldr :: (a -> b -> b) -> b -> t a -> b
  ...
    -- Defined in ‘Data.Foldable’
foldl (+) 0 [1,2,3,4,5,6,7]
28
foldr (+) 0 [1,2,3,4,5,6,7]
28
-- IN THIS CASE, does it make a difference?
-- (try constructing the fold by hand)
```

#### Lists: Either

```haskell
Prelude> :i Either
type Either :: * -> * -> *
data Either a b = Left a | Right b
  	-- Defined in ‘Data.Either’
instance Applicative (Either e) -- Defined in ‘Data.Either’
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in ‘Data.Either’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Monad (Either e) -- Defined in ‘Data.Either’
instance (Ord a, Ord b) => Ord (Either a b)
  -- Defined in ‘Data.Either’
instance Semigroup (Either a b) -- Defined in ‘Data.Either’
instance (Show a, Show b) => Show (Either a b)
  -- Defined in ‘Data.Either’
instance (Read a, Read b) => Read (Either a b)
  -- Defined in ‘Data.Either’
instance Foldable (Either a) -- Defined in ‘Data.Foldable’
instance Traversable (Either a) -- Defined in ‘Data.Traversable’
Prelude> [Left True, Right "Hello", Right "World"]
[Left True,Right "Hello",Right "World"]
Prelude> :t [Left True, Right "Hello", Right "World"]
[Left True, Right "Hello", Right "World"] :: [Either Bool [Char]]
Prelude>
```

#### Lists: Elem Operations, Append, Reverse, Filter

```haskell
data [a] = []
         | a : [a]
         deriving (Show, Eq)
```

```haskell
Prelude> :{
Prelude| elem :: Eq a  => a -> [a] -> Bool
Prelude| elem x []     = False
Prelude| elem x (y:ys) = x == y || elem x ys
Prelude| :}
Prelude> elem 10 [1, 0, 0, 0, 0]
False
Prelude> elem 10 [1, 0, 0, 0, 10]
True
Prelude> -- appending two lists
Prelude> :{
Prelude| (+++) :: [a] -> [a] -> [a]
Prelude| (+++) []        ys   = ys
Prelude| (+++) (x:xs)    ys   = x : (xs ++ ys)
Prelude| :}
Prelude> [1,2,3,4,5] +++ [6,7,8,9,10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> trippleAppend = ([1,2,3,4,5] +++ [6,7,8,9,10])
Prelude> doubleAppend  = ([1,2,3,4,5] ++  [6,7,8,9,10])
Prelude> trippleAppend == doubleAppend
True
Prelude> -- reverse a list
Prelude> :{
Prelude| rev :: [a] -> [a]
Prelude| rev [] = []
Prelude| rev (x:xs) = rev xs ++ [x]
Prelude| :}
Prelude> rev [1,2,3,4,5]
[5,4,3,2,1]
Prelude> -- filter a list based on a rule
Prelude> :{
Prelude| filt :: (a -> Bool) -> [a] -> [a]
Prelude| filt el []        =    []
Prelude| filt el (x:xs)
Prelude|   | el x          = x : filt el xs
Prelude|   | otherwise     =     filt el xs
Prelude| :}
Prelude> filt (\x -> x == 42) [1, 7, 49, 42, 77]
[42]
Prelude> filt (\x -> x /= 42) [1, 7, 49, 42, 77]
[1,7,49,77]
Prelude> filt even [1,2,3,4,5,6,7,8,9]
[2,4,6,8]
Prelude> filt (\x -> length x < 2) [[1,2,3,4,5,6,7,8,9,10],[11],[12],[13,14,15]]
[[11],[12]]
```

#### Additional Notes: Append and the GHC Implementation

```haskell
-- --
-- reimplementation of append (++)
-- appending a list to an empty list results in the init list
-- appending a list to a non-empty list can continously cons the
-- head from the first list to the second list
-- think about how this is working...
-- (xs +++ ys) takes presedence over (:)
-- it's obviously a recusive call (this is Haskell after all)
-- and it would seem that we're essentially removing each head
-- from the first list until we reach the empty list, at which
-- point (:) starts to cons each head from xs to ys on the way
-- back up the call stack?
-- seems a little strange, curious if this is how it's implemented
-- within GHC, I'll go for a look...
-- --

:{
  (+++) :: [a] -> [a] -> [a]
  (+++) []     ys  =  ys
  (+++) (x:xs) ys  =  x : (xs +++ ys)
:}

-- --
-- Ended up down a rabbit hole, if I recall, (++) is synonymous to (<>) in
-- GHC/Base.hs, but can be used infix? Anyway, I ended up in some document
-- on Scholar looking at the implementation as follows:
-- -- from Append.hs (apparently)
-- (++) :: [a] -> [a] -> [a]
-- (x:xs) ++ ys = x : xs ++ ys
-- _      ++ ys = ys
-- --

:{
(++++) :: [a] -> [a] -> [a]
(x:xs) ++++ ys = x : xs ++++ ys
_      ++++ ys = ys
:}

-- --
-- Let's give it a shot!
-- --

-- Prelude> [1,2,3,4,5] ++++ [1,2,3,4,5]
-- [1,2,3,4,5,1,2,3,4,5]
-- Prelude> [] ++++ [1,2,3,4,5]
-- [1,2,3,4,5]
-- Prelude> [1,2,3,4,5] ++++ []
-- [1,2,3,4,5]

-- --
-- Okay. So this is written in a slightly different style.
-- My main concern / question is: in our first implementation
-- we assigned precedence to append before cons. However,
-- within this implementation (from GHC) it doesn't appear
-- to me that precedence has been assigned to append, but it
-- has been assigned to cons (:i (++++), :i (:), :i (++)).
-- (EDIT: actually, I think (++) and (:) are both the same precedence)
-- I suppose a further question would be, if there is no
-- precedence specified for an operator / function, how can
-- Haskell (in this case) know whether to apply : or ++++ first?
-- though, it does seem to work, and IT WOULD SEEM (I am NO AUTHORITY)
-- on this matter WHATSOEVER, but it would seem that pattern matching
-- it 'backwards' as is implemented within GHC would be better?
-- My reasoning may very well be wrong, but my thoughts are:
-- you're recursively (:)'ing the first list and then doing
-- the same the second list, until the list is complete.
-- within our implementation, we're stretching down to the
-- bottom of the call stack before re-cons'ing the list
-- on the way back up?
-- again, may be way off -- I don't really know what I'm
-- talking about, but this is my intuition.
-- --
-- At the end of the day, the compiler is cleverer than I am
-- I assume it makes no difference?
-- --
```

#### Some Tomfoolery

```haskell
-- The proper implementation...

:{
  rList :: [a] -> [a]
  rList []         =  []
  rList (x:xs)     =  rList xs ++ [x]
:}

-- --
-- Me messing around:
-- --

:{
  rL :: [a] -> [a]
  rL []     = []
  rL xs = last xs : rL (init xs)
:}

-- --
-- Messing about:
-- --
-- Prelude> :{
-- Prelude|   rList :: [a] -> [a]
-- Prelude|   rList []         =  []
-- Prelude|   rList (x:xs)     =  rList xs ++ [x]
-- Prelude| :}
-- Prelude> rList [1,2,3,4,5]
-- [5,4,3,2,1]
-- Prelude> :{
-- Prelude|   rL :: [a] -> [a]
-- Prelude|   rL []     = []
-- Prelude|   rL xs = last xs : init xs
-- Prelude| :}
-- Prelude> rL [1,2,3,4,5]
-- [5,1,2,3,4]
-- Prelude>
-- Prelude> :{
-- Prelude|   rL :: [a] -> [a]
-- Prelude|   rL []     = []
-- Prelude|   rL xs = last xs : rL (init xs)
-- Prelude| :}
-- Prelude> rL [1,2,3,4]
-- [4,3,2,1]
-- Prelude> revList = [1,2,3,4,5]
-- Prelude> rList revList == rL revList
-- True
-- --
```
*I have this tendency to spend FAR TOO LONG on simple things... As if I need a serious understanding of THE WHOLE functions, so I think that's enough on lists for now...*

### Summary

Within this lecture we covered:

* Cons
* Breaking Lists Down With Cons
* List Constructors
* Functions On Lists
* Either
* Elem
* Append
* Reverse
* Filter