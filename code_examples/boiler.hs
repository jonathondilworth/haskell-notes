-- boilerplate code built from lectures
-- hacked together whilst following along
-- meant for GHCi 

-- this code is PRETTY MUCH what you will find within my notes
-- most of it has been taken from the IOG lecture series
-- but some has been added, improvised on and changed

-- You can literally copy and paste this 'boilerplate' into GHCi
-- and you should have no errors, though some functions and data
-- structures are defined more than once. Thus, you may want to
-- copy and paste different parts at different times.

-- ANY AND ALL FEEDBACK IS WELCOME

-- You will find some notes in here too, small comments to try and make the
-- reader (in addition to myself) think.

-- !! INTRO !! --

:{
type Txs = Int
data Chain = GenesisBlock
           | Block Chain Txs
           deriving(Show, Eq)

chainLength :: Chain -> Int
chainLength GenesisBlock = 0
chainLength (Block c _)  =
  chainLength c + 1

(!>) :: Chain -> Txs -> Chain
(!>) = Block
infixl 5 !>

hasBlock x GenesisBlock = False
hasBlock x (Block c t) =
  x == t || hasBlock x c

hasBlockProperty :: (Txs -> Bool) -> Chain -> Bool
hasBlockProperty p GenesisBlock =   False
hasBlockProperty p (Block c t)  =   
  p t || hasBlockProperty p c
  
-- Let's not forget my function!

chainTxs :: Chain -> [Txs]
chainTxs GenesisBlock = []
chainTxs (Block c t)  = 
  [t] ++ chainTxs c

:}
:{
-- abstract from the type of transactions
data Chain txs = GenesisBlock
               | Block (Chain txs) txs
               deriving(Show, Eq)

build :: Int -> Chain Int
build n =
  if n <= 0
    then GenesisBlock
    else Block (build (n - 1)) n
:}


-- !! MAYBE !! --

:{
  
  -- Maybe is included within the prelude
  -- check with :i Maybe
  -- ..
  --data Maybe a = Nothing
  --             | Just a
  --             deriving (Show)
  
  -- Maybe Examples

  fromMaybe :: a -> Maybe a -> a
  fromMaybe def Nothing  =  def
  fromMaybe _  (Just a)  =  a

  ifthenelse :: Bool -> a -> a -> a
  ifthenelse False _t e = e
  ifthenelse True  t _e = t

  orElse :: Maybe a -> Maybe a -> Maybe a
  orElse Nothing  y = y
  orElse (Just x) _ = Just x

  mapMaybe :: (a -> b) -> Maybe a -> Maybe b
  mapMaybe f (Just a) = Just (f a)
  mapMaybe _ Nothing  = Nothing

  exHead :: String -> Char
  exHead (x:xs) = x

:}

-- Messing About:
-- --
-- mapMaybe exHead (Just "Hello")
-- mapMaybe exHead Nothing
-- exHead <$> (Just "Hello")
-- exHead <$> Nothing
-- fmap exHead (Just "Hello")
-- fmap exHead Nothing
-- --

:{

  -- Additional Example(s)

  addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
  addMaybe (Just p) (Just q)    =    Just (p + q)
  addMaybe _        _           =    Nothing

  -- Function Mapping (With Maybe) For Two Optional Types

  liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  liftMaybe f (Just x) (Just y) = Just (f x y)
  liftMaybe _ _        _        = Nothing

:}

-- !! PAIRS !! --

:{
  -- 'getters'
  -- get the first component within a key, value pair (k, v)
  gk :: (k, v) -> k
  gk (k, v) = k
  gv :: (k, v) -> v
  gv (k, v) = v
  -- swaping keys for values
  swap :: (k, v) -> (v, k)
  swap (k, v) = (v, k)
:}

-- Messing About:
-- --
-- gk (42, 24)
-- gv (42, 24)
-- kvp = (42, 24)
-- gk kvp
-- gv kvp
-- swap (42, 24)
-- swap kvp
-- swap (swap kvp)
-- swap (swap kvp) == kvp
-- --

-- !! CURRYING !! --

:{
  curr :: ((a, b) -> c) -> a -> b -> c
  curr f a b = f (a, b)
:}

-- --
-- curr swap 4 '2'
-- curr swap "2" '4'
-- --

-- !! FUNCTIONS !! --

-- --
-- Some Notes:
-- --
-- Basic Functions On Lists: Summation
-- It's important to note: the identity of (+) is zero
-- Thus, our terminating recursive case (the empty list) is zero
-- Further, let's eval the formal param (list of Ints) in two ways
-- whilst both are recursive, does the eval order make any difference
-- in this case? Why? See later examples for summation with foldl and foldr
-- when using foldl and foldr does it matter which is used for summation?
-- How about other functions, like... division (doubles, we're using Num => a)
-- or... can you think of other functions where the slightest change in the
-- implementation of the recursive call would make a difference?
-- --
-- food for thought.
-- --

:{
  funcLS :: Num a => [a] -> a
  funcLS [] = 0
  funcLS (x:xs) = x + funcLS xs
  funcRS :: Num a => [a] -> a
  funcRS [] = 0
  funcRS (x:xs) = funcRS xs + x
:}

-- Messing About:
-- --
-- funcLS [1,2,3,4,5,6,7]
-- 28
-- funcRS [1,2,3,4,5,6,7]
-- 28

-- okay, premature optimisation is the root of all evil, right?
-- but, which do you think is the most efficient?

-- :i foldl
-- type Foldable :: (* -> *) -> Constraint
-- class Foldable t where
--   ...
--   foldl :: (b -> a -> b) -> b -> t a -> b
--   ...
--     -- Defined in ‘Data.Foldable’
-- :i foldr
-- type Foldable :: (* -> *) -> Constraint
-- class Foldable t where
--   ...
--   foldr :: (a -> b -> b) -> b -> t a -> b
--   ...
--     -- Defined in ‘Data.Foldable’

-- foldl (+) 0 [1,2,3,4,5,6,7]
-- 28
-- foldr (+) 0 [1,2,3,4,5,6,7]
-- 28
-- --

-- --
-- :i Either
-- --

:{
  eleExists :: Eq a  => a -> [a] -> Bool
  eleExists x []     =  False
  eleExists x (y:ys) = x == y || eleExists x ys 
:}

-- --
-- eleExists 10 [1, 0, 0, 0, 500, 1250]
-- False
-- eleExists 10 [1, 0, -5, 10]
-- True
-- eleExists 10 [1, 0, -5, 10, 10, 42, 10, 10, 42]
-- True
-- --

-- !! APPEND !! --

-- --
-- Some More Notes: Food For Thought
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
-- we assigned presedence to append before cons. However,
-- within this implementation (from GHC) it doesn't appear
-- to me that presedence has been assigned to append, but it
-- has been assigned to cons (:i (++++), :i (:), :i (++)).
-- I suppose a further question would be, if there is no
-- presedence specified for an operator / function, how can
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
-- Food For Thought
-- --

-- !! LISTS !! --

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

-- !! FILTER !! --

:{
  filt :: (a -> Bool) -> [a] -> [a]
  filt el []          =  []
  filt el (x:xs)
    |  el x           =  x : filt el xs
    |  otherwise      =      filt el xs
:} 

-- --
-- Prelude> filt (\x -> x == 42) [1, 7, 49, 42, 77]
-- [42]
-- Prelude> filt (\x -> x /= 42) [1, 7, 49, 42, 77]
-- [1,7,49,77]
-- Prelude> filt even [1, 7, 49, 42, 77]
-- [42]
-- Prelude> filt even [1,2,3,4,5,6,7,8,9]
-- [2,4,6,8]
-- Prelude> filt (\x -> length x < 3) [[1,2,3,4,5,6,7],[42,42,42,42],[42,24,42],[24,24],[42,42]]
-- [[24,24],[42,42]]
-- Prelude> filt (\x -> length x > 3) [[1,2,3,4,5,6,7],[42,42,42,42],[42,24,42],[24,24],[42,42]]
-- [[1,2,3,4,5,6,7],[42,42,42,42]]
-- --

-- --
-- Lambda expressions are pretty handy & nice when it comes to filter
-- --

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

:{

  data Transaction = Transaction Amt Acc Acc
                       deriving (Eq, Show)
  
  type Amt = Int
  type Acc = String
  
  txValue  :: Transaction -> Int
  txValue  (Transaction txV _ _) = txV
  
  txSender :: Transaction -> String
  txSender (Transaction _ txS _) = txS
  
  txRec    :: Transaction -> String
  txRec    (Transaction _ _ txR) = txR

:}

-- --
-- Prelude> Transaction 100 "Jon" "Dee"
-- Transaction 100 "Jon" "Dee"
-- Prelude> tx = Transaction 100 "Jon" "Dee"
-- Prelude> tx
-- Transaction 100 "Jon" "Dee"
-- Prelude> txValue tx
-- 100
-- Prelude> txSender tx
-- "Jon"
-- Prelude> txRec tx
-- "Dee"
-- --

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
-- ...hang on, I got it, immuntability, hence the nested insert calls..
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