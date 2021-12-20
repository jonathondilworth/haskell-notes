### IOHKEDU (023): Lookup Tables

#### Introduction

##### Type Synonyms

*Note: This was something that originally confused me due to the nomenclature. I was getting mixed up between the correct use of: Type Constructor and Data Constructor.*

The ```type``` keyword is used to indicate a synonymous relationship between a newly defined token (name) to represent an existing Type. For example:

```haskell
-- type synonym
type Table k v = [(k,v)]
-- where as a type constructor begins with the data keyword
-- which is somewhat confusing to begin with, e.g.
data Trie a b c = ...
                | ...
                | ...
                deriving (Show, ...)
```

#### Table and Table Functions

*Note: I've paused the video before any implementation and commented on how I would likely think about each of these operations.*

<details>

<summary>If you would like to consider the following intuition before watching the lecture, it may be interesting to see how I (as an individual learning FP) am thinking.</summary>

```haskell
-- lookup table Type synonym
type Table k v = [(k,v)]
-- functions to implement operations on any lookup tables
empty  :: Table k v
insert :: k -> v -> Table k v -> Table k v
delete :: Eq k => k -> Table k v -> Table k v
lookup :: Eq k => k -> Table k v -> Maybe v
-- I would likely implement these as follows
-- empty  = []
-- insert: create a list of size 1 with pair (k,v) and append
-- delete x _ t = filter x (\x -> fc x \= k) t
--                  where
--                    fc (k,v) = k
-- lookup x _ t = sc (filter x (\x -> fc x == k) t)
--                  where
--                    fc (k,v) = k
--                    sc (k,v) = v
-- 
-- ^ That is my intuition, which I'll leave here.
```

*EDIT: As you can see, I made the same mistake of writing the function signature for delete correctly, but then defining the function as having a value, which I could see would not be used, so I dropped a _ in place of it. Quite an easy mistake to make. I was kind of on the right track, but not quite.*

</details>

Since reviewing the lecture, we have some of the following code:

```haskell
Prelude> :{
Prelude| type Table k v = [(k,v)]
Prelude| :}
Prelude> :{
Prelude| empty :: Table k v
Prelude| empty = []
Prelude| :}
Prelude> tbl = empty
Prelude> tbl
[]
Prelude> :{
Prelude| insert :: k -> v -> Table k v -> Table k v
Prelude| insert k v t = (k,v) : t
Prelude| :}
Prelude> tbl' = insert "hello" "world" tbl
Prelude> tbl'
[("hello","world")]
Prelude> -- I made a recursive call which exploded GHCi
Prelude> -- ...
Prelude> etbl = empty
Prelude> etbl
[]
Prelude> tbl' = empty
Prelude> tbl'
[]
Prelude> tbl' = insert "hello" "world" etbl
Prelude> tbl'
[("hello","world")]
Prelude> tbl'' = insert "brave" "new" tbl'
Prelude> tbl''
[("brave","new"),("hello","world")]
Prelude> tbl''' = insert "new" "world" tbl''
Prelude> tbl'''
[("new","world"),("brave","new"),("hello","world")]
Prelude> :{
Prelude| delete :: Eq k => k -> Table k v -> Table k v
Prelude| delete _ [] = []
Prelude| delete k ((k', v') : kvs)
Prelude|   | k == k'        = delete k kvs
Prelude|   | otherwise      = (k', v') : delete k kvs
Prelude| :}
Prelude> delete "new" tbl'''
[("brave","new"),("hello","world")]
Prelude> tbl'''
[("new","world"),("brave","new"),("hello","world")]
Prelude> tbl'''' = insert "hello" "you" tbl'''
Prelude> tbl''''
[("hello","you"),("new","world"),("brave","new"),("hello","world")]
Prelude> delete "hello" tbl''''
[("new","world"),("brave","new")]
Prelude> 
```

Now it looks like we're going to implement ```delete``` similar to how I was thinking...

**Note: REMEMBER ABOUT TYPE HOLES! They're very useful in instances such as this.**

Remember it is considered good practice to implement functions in different ways and see if equality holds using the same arguments for both implementations.

```haskell
Prelude> :{
Prelude| -- We need: ((k,v) -> Bool), or (Pair -> Bool).
Prelude| delete :: Eq k => k -> Table k v -> Table k v
Prelude| delete k kvs = filter (\(k',_) -> not (k == k')) kvs
Prelude|
Prelude| -- how about: ?
Prelude| delete' :: Eq k => k -> Table k v -> Table k v
Prelude| delete' k kvs = filter (\(k',_) -> k /= k') kvs
Prelude| :}
Prelude> tbl''''
[("hello","you"),("new","world"),("brave","new"),("hello","world")]
Prelude> delete' "hello" tbl'''' == delete "hello" tbl''''
True
```
Now let's implement lookup as per the lecture:

```haskell
Prelude> :{
Prelude| lookup' :: Eq k => k -> Table k v -> Maybe v
Prelude| lookup' _ [] = Nothing
Prelude| lookup' k ((k', v') : kvs)
Prelude|   |  k' == k = Just v'
Prelude|   |  otherwise = lookup' k kvs
Prelude| :}
```

So, we've got a bit of a problem IMO, when we run lookup against a list of key->value pairs, it only returns the first pair value with the search key...

Example:

```haskell
Prelude> lookup' "hello" tbl'''
Just "world"
Prelude> lookup' "hello" tbl''''
Just "you"
Prelude> tbl'''
[("new","world"),("brave","new"),("hello","world")]
Prelude> tbl''''
[("hello","you"),("new","world"),("brave","new"),("hello","world")]
Prelude>
```

Then again, I suppose keys should be unique.

**Prepare for even more language constructs and nuances.**

Okay, so as mentioned, the ```type``` keyword is not a constructor, it is a **type synonym**. Data constructors appear to be *typically used* as (possibly parameterised type constructors). **BUT.** There is another option for Type Constructors called ```newtype```, however, it only works for Types with **one constructor and one argument.**

VERY IMPORTANT TO REMEMBER: when using newtype, the type token (or name) on the LHS IS THE TYPE. The token on the RHS is the **(one and only)** constructor, which is probably why it has the same name.

> "If you stumble on syntax, everything else becomes difficult." <br />
— Simon Peyton Jones

```haskell
-- newtypes are faster than using data
Prelude> newtype Table k v = Table [(k,v)]
```

*This whole time I was under the impression that pre-mature optimisation was the root of all evil!*

Further, since the newtype Table k v is no longer a type synonym, we cannot simply 'find and replace' Table k v with the 'newtype' as they're not exactly the same thing.

**This is a pretty long lecture - I was hoping to finish up week two this morning, but it may have to wait.**

**TODO: Finish up week two.**
**TODO: Implement homework exercises thus far.**

*Lecture Time: 22:21*

*...*


