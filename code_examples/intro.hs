module Chains where

{-
  Exercise for week one (homework); I've not looked at the solutions, I'm fair-
  ly certain you'll be able to tell by looking at some of my code! For example
  in some of the tasks I have implemented things in a... not so haskell-esque
  fashion. e.g. in task-2: defining my own helper function and using fmap
  or <$> to map sum across another function mapping (weird, I know)...
  It did however put me on the right track to finding the correct answer.
-}

-- This is the definition of chains from the slides. We omit 'Foldable' from
-- the derived classes, because some of the tasks are intended to let you
-- manually reimplement some of the functions that would be given to you for
-- free by having 'Foldable' derived.

data Chain txs = GenesisBlock
  | Block (Chain txs) txs
  deriving (Show)

eqChain :: Eq txs => Chain txs -> Chain txs -> Bool
eqChain GenesisBlock    GenesisBlock    = True
eqChain (Block c1 txs1) (Block c2 txs2) = eqChain c1 c2 && txs1 == txs2
eqChain _               _               = False

instance Eq txs => Eq (Chain txs) where
  (==) = eqChain

-- More convenient infix operator to build chains, as shown on the slides.
-- Note that you cannot use this operator in patterns (there is a language
-- extension that allows this, but that's a topic for later).

(|>) :: Chain txs -> txs -> Chain txs
(|>) = Block
infixl 5 |>

-- Some example chains

chain1 :: Chain Int
chain1 = GenesisBlock |> 2

chain2 :: Chain Int
chain2 = chain1 |> 4

chain3 :: Chain Int
chain3 = GenesisBlock |> 2 |> 8 |> 3

chain4 :: Chain Int
chain4 = GenesisBlock |> 2 |> 8 |> 4

-- All four chains in a list
chains :: [Chain Int]
chains = [chain1, chain2, chain3, chain4]

-- Task Chains-1.
--
-- Compute the length of a 'Chain'.


{-  
    the same implementation as shown in the lectures...
    it doesn't matter if the chain is parameterised using
    a polymorphic type or not, since we're not checking
    a property tied to txs, we simply need to recursively
    parse the chain and count each call...
    Question threw me off a little, I thought there would
    be some kind of 'curve ball'
-}

lengthChain :: Chain txs -> Int
lengthChain GenesisBlock  = 0
lengthChain (Block c _)   = lengthChain c + 1

propLengthChain1 :: Bool
propLengthChain1 = lengthChain chain1 == 1

propLengthChain2 :: Bool
propLengthChain2 = lengthChain chain2 == 2

propLengthChain3 :: Bool
propLengthChain3 = lengthChain chain3 == 3

propLengthChain4 :: Bool
propLengthChain4 = lengthChain chain4 == 3

-- Same as the above four properties in a single property.
propLengthChain5 :: Bool
propLengthChain5 =
  map lengthChain chains == [1, 2, 3, 3]

-- Task Chains-2.
--
-- Sum all entries in an integer chain.

{-
  implementation one (which I know is a little... wonky):
-}

chainVal :: Chain Int -> [Int]
chainVal GenesisBlock  =  []
chainVal (Block c val) =  [val] ++ chainVal c

sumChains = sum <$> (map chainVal chains)
-- sumChains == [2, 6, 13, 14] 
-- ^ taken from: map sumChain chains == [2, 6, 13, 14]
-- this does work, but I admit it's not implemented as it should be...

{-
  implementation two:
-}

sumChain :: Chain Int -> Int
sumChain GenesisBlock  =  0
sumChain (Block c txs) =  txs + sumChain c

propSumChain1 :: Bool
propSumChain1 = sumChain chain1 == 2

propSumChain2 :: Bool
propSumChain2 = sumChain chain2 == 6

propSumChain3 :: Bool
propSumChain3 = sumChain chain3 == 13

propSumChain4 :: Bool
propSumChain4 = sumChain chain4 == 14

-- Same as the above four properties in a single property.
propSumChain5 :: Bool
propSumChain5 =
  map sumChain chains == [2, 6, 13, 14]

{-
  a quick addition from myself...
-}

propSumChain6 :: Bool
propSumChain6 =
  map sumChain chains == sumChains

-- Task Chains-3.
--
-- Find the maximum element in an integer chain.
-- You can assume for this tasks that all integers
-- in a chain are positive, and that the maximum
-- of an empty chain is 0.

maxChain :: Chain Int -> Int
maxChain x = maximum (chainVal x)
-- see lines 123 -> 125 for chainVal function
-- you can also accomplish the same using:

propMaxChain' :: Bool
propMaxChain' = (maximum <$> (map chainVal chains)) == [2, 4, 8, 8]

maxChains' = maximum <$> (map chainVal chains)

-- a much nicer LEGITIMATE implementation, someone tell me if it's
-- not as it should be and: why :)

maxChain' :: Chain Int -> Int
maxChain' x = maximum (maxIntChain x)
  where
    maxIntChain :: Chain Int -> [Int]
    maxIntChain GenesisBlock   =   []
    maxIntChain (Block ch tx)  =   [tx] ++ maxIntChain ch

propMaxChain :: Bool
propMaxChain =
  map maxChain chains == [2, 4, 8, 8]

{-
  Prelude> maxChains' == map maxChain chains
  True
-}

-- &&

{-
  Prelude> :{
  Prelude| maxChain' :: Chain Int -> Int
  Prelude| maxChain' x = maximum (maxIntChain x)
  Prelude|   where
  Prelude|     maxIntChain :: Chain Int -> [Int]
  Prelude|     maxIntChain GenesisBlock   =   []
  Prelude|     maxIntChain (Block ch tx)  =   [tx] ++ maxIntChain ch
  Prelude| :}
  Prelude> maxChain' chain1
  Prelude> :{
  Prelude| propMaxChain'' :: Bool
  Prelude| propMaxChain'' =  map maxChain' chains == [2, 4, 8, 8]
  Prelude| :}
  Prelude> propMaxChain''
  True
-}

-- Task Chains-4.
--
-- Return the longer of two chains.
-- If both chains have the same length, return
-- the first.

longerChain :: Chain txs -> Chain txs -> Chain txs
longerChain x y = if (lengthChain x) >= (lengthChain y) then x else y

propLongerChain1 :: Bool
propLongerChain1 = longerChain chain1 chain2 == chain2

propLongerChain2 :: Bool
propLongerChain2 = longerChain chain2 chain1 == chain2

propLongerChain3 :: Bool
propLongerChain3 = longerChain chain2 chain3 == chain3

propLongerChain4 :: Bool
propLongerChain4 = longerChain chain3 chain4 == chain3

-- modified propLengthChain2, 3 and 4 to propLongerChain2, 3 and 4
-- if this prop was used during testing, the test would be 'invalid'

propLongerChain5 :: Bool
propLongerChain5 = and [ propLongerChain1
                       , propLongerChain2
                       , propLongerChain3
                       , propLongerChain4
                       ]

-- Task Chains-5.
--
-- Let's call an integer chain "valid" if from the genesis
-- block, each transaction has a higher number than all
-- preceding transactions. (You may assume that all integers
-- are positive.) Check that a given chain is valid.

validChain :: Chain Int -> Bool
validChain cha = satisfyAsc (chainList cha)
  where
    chainList :: Chain Int -> [Int]
    chainList GenesisBlock   =   []
    chainList (Block c tx)   =   [tx] ++ chainList c
    satisfyAsc :: (Ord x)    =>  [x]  -> Bool
    satisfyAsc []            =   True
    satisfyAsc [x]           =   True
    satisfyAsc (x:y:xs)      =   if x > y then satisfyAsc (y:xs) else False

propValidChain1 :: Bool
propValidChain1 = validChain GenesisBlock

propValidChain2 :: Bool
propValidChain2 =
  map validChain chains == [True, True, False, False]

-- Task Chains-6.
--
-- Given two chains, find out whether the first is a prefix
-- of the second. If two chains are equal, they still count
-- as a prefix of each other.
--
-- HINT: This one is a bit tricky.
-- Try to think about which cases are required. Use
-- equality of chains where appropriate. Do not worry about
-- performance or doing too much work. If all fails, skip
-- to task 9.

-- I know this is going to be heavily critised, but I'm just
-- not quite there yet with the function programming mindset? 

-- (an attempt): my implementation

{-
isPrefixOf :: Chain Int -> Chain Int -> Bool
isPrefixOf chain1 chain2 = length (findCommonPre (cList chain1) (cList chain2)) > 0
  where
    cList :: Chain Int -> [Int]
    cList GenesisBlock = []
    cList (Block c tx) = (cList c) ++ [tx]
    findCommonPre :: [Int] -> [Int] -> [Int]
    findCommonPre [] _ = []
    findCommonPre _ [] = []
    findCommonPre (x:xs) (y:ys) = if x == y then [x] ++ (findCommonPre xs ys) else []
-}

-- this is not correct ^ I guess I'm still thinking kind of procedurally; and
-- it's not even nice to read or concise, this is pretty tough for a week one
-- exercise?

-- there must be a simpler recursive call, I'm just looking at this and
-- thinking I must be really stupid

-- which cases are required? in the most base case...
-- params:
-- _ _
-- chain _
-- - chain -- and
-- chain chain ?

-- okay, so txs is a polymorphic type that derives Eq, meaning txs is comparable
-- so, _ _ would be false
-- chain _ would be false
-- _ chain would be false
-- gen gen would be true actually
-- chain chain needs to compare each tx from txs, but thats like a bubble sort almost?
-- that can't be right

-- temporarily skip, GOTO 9

-- I'm fed up of this for now, come back to this tomorrow.

-- isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
-- isPrefixOf = error "TODO: implement isPrefixOf"

-- isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
-- isPrefixOf GenesisBlock  GenesisBlock   =   True
-- isPrefixOf (Block ch tx) _              =   False
-- so, if we're passing two chains, we can pass a chain and a block?

-- we don't actually need the length of the prefix, so surly...
-- the minimum length prefix is 1, so if the first elem from
-- chain 1 is equal to the first elem of chain two, return true?
-- tx1 == tx2

{-

isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
isPrefixOf GenesisBlock  GenesisBlock    =   True
isPrefixOf (Block c1 tx1) (Block c2 tx2) =   tx1 == tx2

-}

-- Prelude| isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
-- Prelude| isPrefixOf GenesisBlock  GenesisBlock    =   True
-- Prelude| isPrefixOf (Block c1 tx1) (Block c2 tx2) =   tx1 == tx2
-- Prelude| :}
-- Prelude> isPrefixOf chain3 ch
-- ch3        ch4        chain1     chain2     chain3     chain4     chainList  chainVal   chains
-- Prelude> isPrefixOf chain3 chain4
-- False
-- Prelude> chain3
-- Block (Block (Block GenesisBlock 2) 8) 3
-- Prelude> chain4
-- Block (Block (Block GenesisBlock 2) 8) 4
-- Prelude> chain1
-- Block GenesisBlock 2
-- Prelude> chain2
-- Block (Block GenesisBlock 2) 4
-- Prelude> isPrefixOf chain2 chain4
-- True
-- Prelude> -- okay, so this is backwards... but we're getting there...

-- to check the first elem...
-- let's not try and do this in any kind of funky way...

-- isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
-- isPrefixOf GenesisBlock   GenesisBlock    =   True
-- isPrefixOf GenesisBlock   _               =   True
-- isPrefixOf _              GenesisBlock    =   False
-- isPrefixOf (Block c1 tx1) (Block c2 tx2)  =   tx1 == tx2 || isPrefixOf c1 c2

-- isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
-- isPrefixOf (Block c1 tx1) (Block c2 tx2)  =   tx1 == tx2 || isPrefixOf c1 c2

-- much harder than I thought it would be.

isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
isPrefixOf = error "TODO: implement isPrefixOf"

propIsPrefixOf1 :: Bool
propIsPrefixOf1 = isPrefixOf chain1 chain2

propIsPrefixOf2 :: Bool
propIsPrefixOf2 = not (isPrefixOf chain2 chain1)

propIsPrefixOf3 :: Bool
propIsPrefixOf3 = isPrefixOf chain2 chain2

propIsPrefixOf4 :: Bool
propIsPrefixOf4 = not (isPrefixOf chain3 chain4)

-- The genesis block is a prefix of any chain.
propIsPrefixOf5 :: Bool
propIsPrefixOf5 =
  all (GenesisBlock `isPrefixOf`) chains

propIsPrefixOf6 :: Bool
propIsPrefixOf6 = and [ propIsPrefixOf1
                      , propIsPrefixOf2
                      , propIsPrefixOf3
                      , propIsPrefixOf4
                      , propIsPrefixOf5
                      ]

-- Task Chains-7.
--
-- Given two chains, find out whether one is a prefix of the
-- other.

areCompatible :: Eq txs => Chain txs -> Chain txs -> Bool
areCompatible = error "TODO: implement areCompatible"

propAreCompatible1 :: Bool
propAreCompatible1 = areCompatible chain1 chain2

propAreCompatible2 :: Bool
propAreCompatible2 = areCompatible chain2 chain1

propAreCompatible3 :: Bool
propAreCompatible3 = not (areCompatible chain3 chain4)

propAreCompatible4 :: Bool
propAreCompatible4 = not (areCompatible chain4 chain3)

-- The genesis block is compatible with any chain.
propAreCompatible5 :: Bool
propAreCompatible5 =
  all (areCompatible GenesisBlock) chains

-- All chains are compatible with the genesis block.
propAreCompatible6 :: Bool
propAreCompatible6 =
  all (\ c -> areCompatible c GenesisBlock) chains

propAreCompatible7 :: Bool
propAreCompatible7 = and [ propAreCompatible1
                         , propAreCompatible2
                         , propAreCompatible3
                         , propAreCompatible4
                         , propAreCompatible5
                         , propAreCompatible6
                         ]

-- Task Chains-8.
--
-- Given two chains, find the longest common prefix.

commonPrefix :: Eq txs => Chain txs -> Chain txs -> Chain txs
commonPrefix = error "TODO: implement commonPrefix"

propCommonPrefix1 :: Bool
propCommonPrefix1 = commonPrefix chain1 chain2 == chain1

propCommonPrefix2 :: Bool
propCommonPrefix2 = commonPrefix chain2 chain1 == chain1

propCommonPrefix3 :: Bool
propCommonPrefix3 = commonPrefix chain1 chain3 == chain1

propCommonPrefix4 :: Bool
propCommonPrefix4 = commonPrefix chain3 chain4 == chain1 |> 8

propCommonPrefix5 :: Bool
propCommonPrefix5 =
  commonPrefix chain3 (GenesisBlock |> 5) == GenesisBlock

-- Task Chains-9.
--
-- Reimplement the hasBlockProp function from the slides
-- for our more general Chain type which is polymorphic
-- in the type of transactions.

hasBlockProp :: Eq txs => (txs -> Bool) -> Chain txs -> Bool
hasBlockProp p GenesisBlock  = False
hasBlockProp p (Block ch tx) = p tx || hasBlockProp p ch

propHasBlockProp1 :: Bool
propHasBlockProp1 = hasBlockProp even chain3

propHasBlockProp2 :: Bool
propHasBlockProp2 = not (hasBlockProp odd chain2)

-- Task Chains-10.
--
-- Reimplement hasBlock in terms of hasBlockProp.

hasBlock :: Eq txs => txs -> Chain txs -> Bool
hasBlock pTx chain = hasBlockProp (\x -> x == pTx) chain

propHasBlock1 :: Bool
propHasBlock1 = hasBlock 8 chain4

propHasBlock2 :: Bool
propHasBlock2 = not (hasBlock 8 chain5)

-- Task Chains-11.
--
-- Check whether all blocks in a chain are unique,
-- i.e., different from each other.

uniqueBlocks :: Eq txs => Chain txs -> Bool
uniqueBlocks = error "TODO: implement uniqueBlocks"

propUniqueBlocks1 :: Bool
propUniqueBlocks1 = uniqueBlocks (GenesisBlock :: Chain Int)

propUniqueBlocks2 :: Bool
propUniqueBlocks2 = uniqueBlocks chain1

propUniqueBlocks3 :: Bool
propUniqueBlocks3 = uniqueBlocks chain6

propUniqueBlocks4 :: Bool
propUniqueBlocks4 = not (uniqueBlocks (Block chain2 2))

-- Task Chains-12.
--
-- Check whether all blocks in the given chain have
-- a particular property.

allBlockProp :: (txs -> Bool) -> Chain txs -> Bool
allBlockProp = error "TODO: implement allBlockProp"

propAllBlockProp1 :: Bool
propAllBlockProp1 = allBlockProp (== 'x') GenesisBlock

propAllBlockProp2 :: Bool
propAllBlockProp2 = allBlockProp even chain2

propAllBlockProp3 :: Bool
propAllBlockProp3 = not (allBlockProp even chain3)

-- Task Chains-13.
--
-- Given a list of chains, determine the maximum length.
-- If the given list is empty, return 0.

maxChainss :: [Chain txs] -> Int
maxChainss xChains = maximum (map length [genTxs x | x <- xChains])
  where
    genTxs :: Chain txs -> [txs]
    genTxs GenesisBlock   =   []
    genTxs (Block ch tx)  =   [tx] ++ genTxs ch

propMaxChains1 :: Bool
propMaxChains1 = maxChainss [] == 0

propMaxChains2 :: Bool
propMaxChains2 = maxChainss [chain1, chain2, chain3] == 3

-- Task Chains-14.
--
-- Given a non-empty list of chains, determine the longest
-- common prefix. We model a non-empty list here as a single
-- element plus a normal list.

longestCommonPrefix :: Eq txs => Chain txs -> [Chain txs] -> Chain txs
longestCommonPrefix = error "TODO: implement longestCommonPrefix"

propLongestCommonPrefix1 :: Bool
propLongestCommonPrefix1 = longestCommonPrefix chain4 [] == chain4

propLongestCommonPrefix2 :: Bool
propLongestCommonPrefix2 = longestCommonPrefix chain2 [chain3] == chain1

propLongestCommonPrefix3 :: Bool
propLongestCommonPrefix3 = longestCommonPrefix chain6 [chain5, chain5] == chain5

-- Task Chains-15.
--
-- Given an integer chain, interpret each integer as a change
-- of the current balance. The genesis block has a balance of 0.
-- The final balance is given by sumChain. Define a function
-- that computes a chain of all the intermediate balances. The
-- resulting chain should have the same length as the original
-- chain, but each entry should be the intermediate balance of
-- the original chain at that point.

balancesChain :: Chain Int -> Chain Int
balancesChain = error "TODO: implement balancedChain"

propBalancesChain1 :: Bool
propBalancesChain1 =
  balancesChain chain1 == chain1

propBalancesChain2 :: Bool
propBalancesChain2 =
  balancesChain chain2 == chain1 |> 6

propBalancesChain3 :: Bool
propBalancesChain3 =
  balancesChain chain3 == chain1 |> 10 |> 13

propBalancesChain4 :: Bool
propBalancesChain4 =
  balancesChain chain4 == chain1 |> 10 |> 14

chain5 :: Chain Int
chain5 = GenesisBlock |> 5 |> (-5)

chain6 :: Chain Int
chain6 = chain5 |> (-1) |> 3

propBalancesChain5 :: Bool
propBalancesChain5 =
  balancesChain chain5 == GenesisBlock |> 5 |> 0

propBalancesChain6 :: Bool
propBalancesChain6 =
  balancesChain chain6 == GenesisBlock |> 5 |> 0 |> (-1) |> 2

propBalancesChain7 :: Bool
propBalancesChain7 = and [ propBalancesChain1
                         , propBalancesChain2
                         , propBalancesChain3
                         , propBalancesChain4
                         , propBalancesChain5
                         , propBalancesChain6
                         ]

-- Task Chains-16.
--
-- Given an integer chain, interpret it as a balances chain
-- as in the previous task and check that none of the
-- intermediate balances are negative.

validBalancesChain :: Chain Int -> Bool
validBalancesChain = error "TODO: implement validBalancesChain"

propValidBalancesChain1 :: Bool
propValidBalancesChain1 =
  all validBalancesChain [chain1, chain2, chain3, chain4, chain5]

propValidBalancesChain2 :: Bool
propValidBalancesChain2 =
  not (validBalancesChain chain6)

propValidBalancesChain3 :: Bool
propValidBalancesChain3 = and [ propValidBalancesChain1
                              , propValidBalancesChain2
                              ]

-- Task Chains-17.
--
-- Drop blocks from the end of the chain as long as the
-- transactions in the blocks fulfill the given property.
-- Return the rest.

shortenWhile :: (txs -> Bool) -> Chain txs -> Chain txs
shortenWhile = error "TODO: implement shortenWhile"

propShortenWhile1 :: Bool
propShortenWhile1 = shortenWhile even chain2 == GenesisBlock

propShortenWhile2 :: Bool
propShortenWhile2 = shortenWhile (> 3) chain2 == chain1

-- Task Chains-18.
--
-- Reimplement the function 'build' from the slides.

build :: Int -> Chain Int
build = error "TODO: implement build"

propBuild1 :: Bool
propBuild1 = lengthChain (build 1000) == 1000

propBuild2 :: Bool
propBuild2 = build (-5) == GenesisBlock

propBuild3 :: Bool
propBuild3 = build 3 == GenesisBlock |> 1 |> 2 |> 3

-- Task Chains-19.
--
-- Produce a chain of given length that contains the
-- given transactions in every block.
--
-- If the given length is zero or negative, return the
-- genesis block.

replicateChain :: Int -> txs -> Chain txs
replicateChain = error "TODO: implement replicateChain"

propReplicateChain1 :: Bool
propReplicateChain1 = replicateChain (-7) 'x' == GenesisBlock

propReplicateChain2 :: Bool
propReplicateChain2 = replicateChain 1 2 == chain1

propReplicateChain3 :: Bool
propReplicateChain3 = replicateChain 3 'x' == GenesisBlock |> 'x' |> 'x' |> 'x'

-- Task Chains-20.
--
-- Implement a function that gives you the prefix of the
-- given length of the given chain. If the chain is too short,
-- the entire chain is returned. If the given length is zero or negative,
-- return the genesis block only.

cutPrefix :: Int -> Chain txs -> Chain txs
cutPrefix = error "TODO: implement cutPrefix"

propCutPrefix1 :: Bool
propCutPrefix1 = cutPrefix 1 chain2 == chain1

propCutPrefix2 :: Bool
propCutPrefix2 = cutPrefix 2 chain2 == chain2

propCutPrefix3 :: Bool
propCutPrefix3 = cutPrefix 0 chain3 == GenesisBlock

propCutPrefix4 :: Bool
propCutPrefix4 = cutPrefix (-7) chain1 == GenesisBlock

propCutPrefix5 :: Bool
propCutPrefix5 = and [ propCutPrefix1
                     , propCommonPrefix2
                     , propCommonPrefix3
                     , propCommonPrefix4
                     ]


-- TODO: Finish 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20