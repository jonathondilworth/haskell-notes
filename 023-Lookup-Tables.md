### IOHKEDU (023): Lookup Tables

#### Introduction

*To Write...*

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

***Authors Note: I had to take about three weeks leave (during the holiday season) due to multiple (quite serious) reasons. I will be online and active as much as possible. This note will be erased when I can return to taking an actively full time role in this community and projects in general.***

*Lecture Time: 22:21 - Resuming by back-tracking, see next commit with ```boiler.hs``` in code_examples.*

```haskell
-- !! TYPE SYNONYMS !! --
-- --
-- Creating a new name for a Type which already exists.
-- In this instance, Table for a list of tuples (or pairs, spec: key, value pairs)
-- --

type Table k v = [(k, v)]

-- !! LESSON (023) ON LOOKUP TABLES !! --

:{
  empty :: Table k v
  empty = []

  insert :: k -> v -> Table k v -> Table k v
  insert k v t = (k, v) : t

  delete :: Eq k => k -> Table k v -> Table k v
  delete k t  = filter (\(k',_) -> k /= k') t

  lookUp :: Eq k => k -> Table k v -> Maybe v
  lookUp _ [] = Nothing
  lookUp k ((k', v') : t)
    |  k' == k   = Just v'
    |  otherwise = lookUp k t
:}

-- --
-- Messing about:
-- --
-- Prelude> :{
-- Prelude| empty :: Table k v
-- Prelude| empty = []
-- Prelude| :}
-- Prelude> tbl = empty
-- Prelude> :{
-- Prelude| insert :: k -> v -> Table k v -> Table k v
-- Prelude| insert k v t = (k, v) : t
-- Prelude| :}
-- Prelude> insert 42 24 tbl
-- [(42,24)]
-- Prelude> nTbl = insert 42 24 tbl
-- Prelude> nTbl
-- [(42,24)]
-- Prelude> :{
-- Prelude| delete :: Eq k => k -> Table k v -> Table k v
-- Prelude| delete k t = filter (\(k',_) -> k /= k') t
-- Prelude| :}
-- Prelude> nnTbl = insert 24 42 nTbl
-- Prelude> nnTbl
-- [(24,42),(42,24)]
-- Prelude> dTbl = delete 24 nnTbl
-- Prelude> dTbl
-- [(42,24)]
-- Prelude> :{
-- Prelude|   empty :: Table k v
-- Prelude|   empty = []
-- Prelude|
-- Prelude|   insert :: k -> v -> Table k v -> Table k v
-- Prelude|   insert k v t = (k, v) : t
-- Prelude|
-- Prelude|   delete :: Eq k => k -> Table k v -> Table k v
-- Prelude|   delete k t = filter (\(k',_) -> k /= k') t
-- Prelude|
-- Prelude|   lookUp :: Eq k => k -> Table k v -> Maybe v
-- Prelude|   lookUp _ [] = Nothing
-- Prelude|   lookUp k ((k', v') : t)
-- Prelude|     |  k' == k   = Just v'
-- Prelude|     |  otherwise = lookUp k t
-- Prelude| :}
-- Prelude> lookUp 42 dTbl
-- Just 24
-- --

-- --
-- data -> new data Type
-- type -> Type synonym
-- newtype -> like data, but only new Types with one cons with 1 arg
-- example: newtype Table k v = Table [(k, v)]
-- However, newtype does what it says, it introduces a new Type
-- Thus, if we executed newtype Table kv = Table [(k, v)]
-- Table is no longer a Type Synonym, it's essentially more explicit
-- --

-- !! MODELLING A TRANSACTION !! --

-- I could see where this was going, already aware of 'record format'
-- Although, both implementations worth messing about with for the
-- sake of learning

Prelude> :{
Prelude|
Prelude| data Transaction = Transaction Amt Acc Acc
Prelude|                      deriving (Eq, Show)
Prelude|
Prelude| type Amt = Int
Prelude| type Acc = String
Prelude|
Prelude| txValue  :: Transaction -> Int
Prelude| txValue  (Transaction txV _ _) = txV
Prelude|
Prelude| txSender :: Transaction -> String
Prelude| txSender (Transaction _ txS _) = txS
Prelude|
Prelude| txRec    :: Transaction -> String
Prelude| txRec    (Transaction _ _ txR) = txR
Prelude|
Prelude| :}
Prelude> Transaction 100 "Jon" "Dee"
Transaction 100 "Jon" "Dee"
Prelude> tx = Transaction 100 "Jon" "Dee"
Prelude> tx
Transaction 100 "Jon" "Dee"
Prelude> txValue tx
100
Prelude> txSender tx
"Jon"
Prelude> txRec tx
"Dee"

:{

data Transaction = Transaction
  {  txValue  :: Int
  ,  txSender :: String
  ,  txRec    :: String
  } deriving (Eq, Show)

:}

-- -- 
-- Prelude> tx = Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> tx
-- Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> let tx = Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> tx
-- Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> txValue tx
-- 100
-- Prelude> txSender tx
-- "Tom"
-- Prelude> txRec tx
-- "Joe"
-- --

:{

-- No different to the previous Transaction record Type definition
-- except insofar as we would be using type synonyms:
-- type Amt = Int & type Acc = String.
-- These type synonyms have already been declared

-- data Transaction = Transaction
--   {  txValue  :: Amt
--   ,  txSender :: Acc
--   ,  txRec    :: Acc
--   } deriving (Eq, Show)

-- Type synonym for Table k v
type Accounts = Table Acc Amt

-- --
-- SERIOUS FOOD FOR THOUGHT
-- --
-- just a quick note, I had to take (only about) 4 minutes or so to figure out
-- what the polymorphic argument a for pTx tx a actually was/is/represents
-- the name 'a' is by convention, I know, but I felt as though I had to back-track
-- "a is of type Accounts, Accounts is a type synonym for Table k v and so a is of
-- type Table, which if we've not declared the newtype, which in this case, we've not
-- then a is of type [(,)], with signature [a -> b -> (a,b)]... So, let's try this
-- without declaring Table k v as a newtype and see what happens, then let's use newtype
-- ... will it make a difference?"
-- --
-- As you can see, fairly mind-bending, or maybe I'm an idiot (highly likely)
-- Genuine question: is it a good idea to have a large linkage of type synonyms?
-- Reason for asking: see the above ^
-- --
-- I would likely rename a to: txTbl
-- --
-- ACTUALLY, since a is of type Accounts, it's no longer as expressive as Table k v
-- since k is of (yet ANOTHER TYPE SYNONYM, but I know it's of type Table String Int
-- OR, if you prefer, Table Acc Amt, we've constricted the expressiveness of Table k v
-- such that k is of type String and v is of type Int; so... a is no longer polymorphic
-- (or to be specific, a is no longer a parameter to a parametrically polymorphic Type 
-- (or data structure?) PLEASE CORRECT ME IF I HAVE THIS MIXXED UP...):
-- it's actually VERY STRICT insofar as the type is Table String Int...
-- --
-- Can you perhaps see why single letter variable names have their positives and negatives?
-- ESP. when you construct a giant chain of type synonyms, is this genuine convention?
-- --
-- Again, I am genuinely not trying to come across as any kind of authority, I'm quite new
-- to FP; and I actually think it's extremely worth-while to learn (thus, this). What I am
-- being is fairly inquizitive (which I'm told is a good thing), and I doubt anyone will read this anyway.
-- --

-- --
-- additional notes...
-- --
-- even though we have the tx, we need to perform a lookup, as the txSender and txRec are keys
-- for the Table k(eys) v(ales) -- which is now Table Acc Amt === Table String Int
-- So.. the txSender & rec are Keys of type String for a Table of type [a -> b -> (a, b)]
-- Where (in this specific case), a is of type String and b is of type Bool
-- We need the co-responding Int values as they're representative of acc balances (I think..)
-- --
-- we don't need to lookUp the value of the tx, as it's passed via the first formal argument
-- within a record of type Transaction, so txValue is easy to get at.
-- --
-- I'm just trying to think like a compiler...
-- I thought it was note-worthy
-- --

-- --
-- I have to rename this 'a' formal param, seriously. Sorry.
-- changed from: pTx tx a =
-- to: pTx tx txTbl
-- --

-- --
-- ...hang on, I got it, immutability, hence the nested insert calls..
-- .. I was originally thinking, would you map these inserts, etc; but yeah..
-- I have tried to make it as readable as possible in its raw form (before 
-- the addition of the Transaction record constructor as a parameter)
-- I hope this works..
-- --

pTx :: Transaction -> Accounts -> Accounts
pTx tx txTbl =
  let txV         = txValue  tx
      txSenderKey = txSender tx
      txRecKey    = txRec    tx
      txSenderBal = fromMaybe 0 (lookUp txSenderKey txTbl)
      txRecBal    = fromMaybe 0 (lookUp txRecKey txTbl)
  in  insert txSenderKey (txSenderBal - txV) ((insert txRecKey (txRecBal + txV)) txTbl)

:}

-- --
-- Prelude> let tx = Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> tx
-- Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> txRec tx
-- "Joe"
-- Prelude> let tx = Transaction {txValue = 100, txSender = "Tom", txRec = "Joe"}
-- Prelude> let txV = txVal
-- txVal    txValue
-- Prelude> let txV = txValue tx
-- Prelude> let txSenderKey = txSender tx
-- Prelude> let txRecKey = txRec tx
-- Prelude> nnTbl
-- [(24,42),(42,24)]
-- Prelude> newTbl = empty
-- Prelude> insert "Joe" 10000 newTbl
-- [("Joe",10000)]
-- Prelude> oldTbl = newTbl
-- Prelude> newTbl = insert "Joe" 10000 oldTbl
-- Prelude> oldTbl = newTbl
-- Prelude> newTbl = insert "Tom" 100 oldTbl
-- Prelude> newTbl
-- [("Tom",100),("Joe",10000)]
-- Prelude> let txSenderBal = fromMaybe 0 (lookUp txSenderKey newTbl)
-- Prelude> txSenderBal
-- 100
-- Prelude> let txRecBal = fromMaybe 0 (lookUp txRecKey newTbl)
-- Prelude> txRecBal
-- 10000
-- Prelude> insert txSenderKey (txSenderBal - txV) ((insert txRecKey (txRecBal + txV)) newTbl)
-- [("Tom",0),("Joe",10100),("Tom",100),("Joe",10000)]
-- -- similarly:
-- Prelude> pTx tx newTbl
-- [("Tom",0),("Joe",10100),("Tom",100),("Joe",10000)]
-- --
```

*Rather long lecture...*