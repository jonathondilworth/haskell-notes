### Higher Order Functions

*This is an immediate follow up to [011-An-Overview-Of-Haskell.md](011-An-Overview-Of-Haskell.md), see notes over there before moving to higher order functions.*

**Higher Order Functions**

* We would like to be able to parse our data structure (our fairytale Blockchain) and search for a block with particular properties.
* For example, is there a block with an even number, is there a Block that is divisible by seven, etc.
* Thus, we would like to know if a constraint (or property) is satisfied given a Block.
* Considering the following type signature: ```Txs -> Bool``` where ```Txs``` is simply a reference (an identifier?) for the constructor Int<sup>[1](#1)</sup>.
* This type signature is specifying a simple function that accepts an Int and returns a Bool, or you can think of it as a mapping from (Int -> Bool). Personally, thinking of it as a mapping (which is exactly what a function is, but whatever) makes it easier to understand higher order functions (the notion of mappings as type signatures and using the guidance of the type system creates this new way of thinking, almost).
* In the context of a higher order (recursive) function:

```haskell
hasBlockProperty :: (Txs -> Bool) -> Chain -> Bool
hasBlockProperty p GenesisBlock   =   False
hasBlockProperty p (Block c t)    =   p t || hasBlockProperty p c
```

* The parentheses around the mapping from ```(Txs -> Bool)``` within the function declaration of ```hasBlockProperty``` indicates that this will be a single argument, described as a function, a test, a property (but I do, for some reasoning like: a mapping) which we're going to label as ```p``` within the set of formal parameters passed to ```hasBlockProperty```. I have a minor concern that prop could lend itself to 'proposition'. And although we're using many single letter parameters, it's not always considered bad practice within Haskell (as far as I understand, so long as the composition of the overall code is readable).

So... What does this function actually do?

Well, let's think about it. There isn't too much point learning something if you don't think about it now is there? A function, which accepts a mapping (does a constraint hold, given...), a fairytale Blockchain and it will terminate by returning a Bool.

Thus, if we define this function mapping, which is a test to see whether or not a constraint holds on an Int. Since the chain is made up of many Ints (Txs) we can parse the chain (in some fashion) and evaluate each Int on the chain as we do!

So, how are we going to parse this 'Chain' data structure? Well, I do like my list comprehensions, but this is Haskell and everyone seems to be mad on recursion, so I guess we'll be doing it recursively! List comprehensions are in fact faster (or should be), but you know, premature optimisation if the root of all evil. Thus, if the property is not satisfied during the first recursive call, we'll simply move through the chain by passing the same function mapping (constraint check) and chain to the function until we eventually reach the genesis block, which is the base case; and if we've not found an instance where this constraint holds, well then, we're going to return false, are we not?

**Yes we are.**

Calling the function: ```hasBlockProperty``` requires us to provide a function to test if a given constraint holds on every Block within our fairytale Blockchain. However, we've not actually defined any type of testable constraint that we can apply to each block (value)... So we've got some options, but since we're writing Haskell, we'll be ```lazy``` and opt to use an 'Anonymous function' otherwise known as a Lambda Expression (which is just a way to define a function on the fly):

```haskell
hasBlockProperty (\ x -> x > 10) chain1 -- returns a Bool
-- accept x and return x > 10 (which is in fact a mapping from 'x'
-- which has to be of type int for the program to compile
-- and then tests to see if x > 10 and returns a bool
-- a mapping from (Int -> Bool), just like we wanted
```

**Other Examples**

```haskell
-- Example Two
hasBlockProperty even chain1
-- Example Three
hasBlockProperty (\ x -> x == 42) chain42
```

**I've Suddenly Decided I Want An Ordered List Of Transactions!**

*Kind of wandering off track here, but hey, I'm enjoying myself...*

Because, hey, why not. Plus List Comprehensions are great.

```haskell
-- this is how I'm going to easily obtain my list of Txs, which I can now use
-- however which way I like!
chainTxs :: Chain -> [Txs]
chainTxs GenesisBlock = []
chainTxs (Block c t)  = [t] ++ chainTxs c
-- let's now test the same properties using a list comprehension!
True `elem` ((\ x -> x > 10) <$> [ x | x <- (chainTxs chain42)])
```

Right. Now I've had my fun, back to the lecture. I know, probably horrible code, I'm learning!

**Alternative Implementation of hasBlockProperty**

```haskell
hasBlockProperty' :: (Txs -> Bool) -> Chain -> Bool
hasBlockProperty' = \p chain ->
  case chain of
    GenesisBlock  -> False
    Block c t     -> p t || hasBlockProperty' p c
```

Always remember that throughout the use of the case style, arrows are used in place of equals, as demonstrated above.

**Functional Composition // Similarity of Functions // Type-Directed Programming**

Consider:

```haskell
type Txs = Int
data Chain = GenesisBlock
           | Block Chain Txs
           deriving(Show, Eq)

chainLength :: Chain -> Int
chainLength GenesisBlock = 0
chainLength (Block c _)  =
  chainLength c + 1

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

```

* We are starting to see a pattern (recursion).
* Functions follow the structure of the data that they operate on.
* It would appear that (in most cases) given a data type, a single function (equation) will exist for each data constructor that belongs to that data type.
* Since Block contains a recursive call, most of the functions operating on chains will make a recursive call to that Chain that occurs recursively within the Block constructor.
* **Programming Principles from the data structures they operate on, functions follow the structure of the data!**
* Type-Directed Programming!

Ouch! What a question -- "Premature Optimisation Is The Root Of All Evil!" -- probably want to strive for correctness before optimisation; so just, don't worry about it until you need to.

**Every Expression in Haskell has a Type**

**Type Inference**

Right, so I was pretty underwhelmed until the inference (Txs -> Bool) was demonstrated as inferable since (||) :: Bool -> Bool -> Bool, thus the result of prop t (or p t) has to be of type Bool. I must say, I found that to be fairly insightful.

It is kind of strange, having such a loose way (or many ways) to define the same function / operation, whilst having such a strict type system at compile time, giving rise to this kind of 'being able to ask the computer for help' is pretty damn powerful!

* Polymorphism
* Parameterised Data Types (Parametric Polymorphism)

```haskell
-- abstract from the type of transactions
data Chain txs = GenesisBlock
               | Block (Chain txs) txs
               deriving(Show, Eq)
```

Within the above example, the element found within the type Block is *Generic*. Explicitly, the ```Chain``` data type is parameterised using the token (name/value) ```txs```. Thus ```Chain``` is a parameterised data type implementing parametric polymorphism (a subset of 'Generics' found across almost all languages).

Furthermore, since this is still a **recursive** *parameterised* data type, the Block constructor (which takes an argument Chain) is also parameterised within this parametric style.

This means that each Block within the Chain can be of any type, so long as for any given chain, the type remains constant. However, given multiple chains, the type can vary across different instances. In fact, this is *so cool* that you could even use a polymorphic type or even a polymorphic data type as the 'value' which occupies txs.

Now, the type signatures of any given data constructor has changed for the parameterised data type ```Chain``` such that they are also now polymorphic:

```haskell
GenesisBlock :: Chain txs
Block :: Chain txs -> txs -> Chain txs
```

*Question: Does the following statement make sense, and since referential transparency is maintained within Haskell (meaning it is forever pure), is this something we should be thinking about when writing our code? Ideally, you want to try your best to maintain data structures and their respective content as 'abstract' as possible before performing any kind of evaluation. Reason being, depending on the evaluation process (the function applied to any given data structure), you may be affecting the precision of the values stored therein (due to the nature of discrete systems). This is why we should consider using algebraic data types such as abstract syntax trees.*

**Footnotes**

<a href="#1" id="1">1</a>. We may consider using Integer which is unbounded, or depending on our model, we may even want to change the implementation such that it handles various types, perhaps it should even be polymorphic, in which case, we may consider using Num.