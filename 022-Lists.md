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