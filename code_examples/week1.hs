{-
  Boilerplate code for week one, overview of Haskell (for GHCi)
  Original Author: Dr. Loh A.
  Changes / Modifications: Dilworth. J
-}
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
-- see notes for examples of use within GHCi