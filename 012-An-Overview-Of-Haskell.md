### Higher Order Functions

*This is an immediate follow up to [011-An-Overview-Of-Haskell.md](011-An-Overview-Of-Haskell.md), see notes over there before moving to higher order functions.*

**Higher Order Functions**

* We would like to be able to parse our data structure (our fairytale Blockchain) and search for a block with particular properties.
* For example, is there a block with an even number, is there a Block that is divisible by seven, etc.
* Thus, we would like to know if a constraint (or property) is satisfied given a Block.
* Considering the following type signature: ```Txs -> Bool``` where ```Txs``` is simply a reference (an identifier?) for the constructor Int<sup>[1](#s1)</sup>.
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

Consider our ```chainLength``` function. It's (not very) interesting to note that regardless of the type used in place of txs, given an 'instance' (or any given) chain, we can compute its length. I suppose this appears to me to be so obvious because we're just counting ```txs``` and it really doesn't matter what ```txs``` are, so long as they remain consistent. Consider the following Q&A:

Question Example One: How many sheep are in that field? (Yes, I am from Wales).

Answer One: Reply by counting ONLY the sheep in the field. It would actually be IMPOSSIBLE for you to count a cow because the compiler should throw an exception. However... What if we changed the question slightly?

Question Example Two: How many ANIMALS (sheep, cow) are in that field?

Answer Two: Reply by counting ONLY the animals that belong to the set of all animals as is described by the question. Thus, reply by providing a summation of ONLY the sheep AND cows.

Now, you can see that ANIMALS could even be extended to include pigs! This is a kind of *toddler example* to try and provide some intuition as to what polymorphism is and why we can use generic (or parametrically polymorphic) recursive functions to do things like count the number of 'elements' within a parameterised data structure *(which may or may not have been composed using a recursively generic data constructor, though it seems likely).*

<details>

<summary>Mathematical Answer For Above Question (Simple Summation)</summary>

Answer: $ \big[ \sum_{i=0}^{N}(1)\ |\ S = \{s_{0}, s_{1}, ..., s_{N}\}, where\ S\ is\ the\ set\ of\ sheep\ \big] $

</details>

**What about ```hasBlockProperty```?**

Yep! Still works, given that the constraint that we're checking for (which can be defined on the fly and **could also be generic in nature**) is type compatible with instances of txs in the chain, see below:

```haskell
hasBlockProperty :: (txs -> Bool) -> Chain txs -> Bool
```

**Quick Note On Haskell Conventions**

* Type Constructors: start with upper case.
* Data Constructors: start with upper case.
* Functions (normal identifiers): camelCase.
* Symbolic Constructors: enclosed within colons, e.g. (:&&&:).

*Note: when I first started programming I never thought I would be using phrases like: a recursive generic data constructor for a parametric polymorphic data type.*

*Question: Does the following statement make sense, and since referential transparency is maintained within Haskell (meaning it is forever pure), is this something we should be thinking about when writing our code? Ideally, you want to try your best to maintain data structures and their respective content as 'abstract' as possible before performing any kind of evaluation. Reason being, depending on the evaluation process (the function applied to any given data structure), you may be affecting the precision of the values stored therein (due to the nature of discrete systems). This is why we should consider using algebraic data types such as abstract syntax trees. (I'm thinking along the lines of: Constraints Liberate, Liberties Constrain; by Runar)*

**Overloading**

Consider the type class Foldable:

```haskell
Prelude Data.List> :t length
length :: Foldable t => t a -> Int
```

<details>

<summary>Here Be Tomfoolery.</summary>

*Lists: (which we'll speak about later, this lecture is a bit like assembly language; I believe academic nomenclature defines the GOTO function as: § with value x.\**).

I've kindly included an example in ARM assembly:

```armasm
LDR    R0, 0x00010072
STR    R1, [R0]
MOV    PC, R1
```

</details>

Lists are Foldable, thus: ```t a``` can be a list and since we return ```Int```, well this makes perfect sense, right? You would expect a function called length to accept a list (with a polymorphic set of elements, such that for all elem, elem type 1 == elem type 2) and return an Integer (an unbounded 'whole' number).

**But what does this have to do with OVERLOADING?**

Well, consider the following function declarations:

```haskell
length    ::    [a]    -> Int -- fine
length    ::    [Int]  -> Int -- fine
length    ::    String -> Int -- fine
```
If you really wanted to, you could modify the type signature of length by re-declaring the function as any of the above, they're all valid. Since: ```String :: [Char]```, a list of integers is foldable, and a list of some type a is foldable.

```haskell
Prelude Data.List> :t elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
Prelude Data.List> :t any
any :: Foldable t => (a -> Bool) -> t a -> Bool
```

*Specialisations:*

```haskell
elem :: (Eq a) => a -> [a] -> Bool
elem :: Txs -> [Txs] -> Bool
any  :: (a -> Bool) -> [a] -> Bool
any  :: (Txs -> Bool) -> [Txs] -> Bool
```

**Checking If Two Values Are Equal**

The data type must derive (and implement) Eq: ```deriving (Eq)```, at the very minimum.

```haskell
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
Prelude>
```

You must choose an instantiation for ```a``` which is a member of the equality class ```Eq``` - you can think of ```Eq a``` as a constraint capturing all the types that support this equality test. This is known as a **Type Class Constraint**.

It's like using ```Num```, see the example below:

```haskell
funcx :: Num p => p -> p -> p
funcx x y = x - y
-- let's test this...
Prelude> :{
Prelude| funcx :: Num p => p -> p -> p
Prelude| funcx x y = x - y
Prelude| :}
Prelude> funcx 1 2
-1
Prelude> funcx 1 0.2
0.8
Prelude> funcx 2 1
1
Prelude> funcx 200 1.71872637521675376162873
198.28127362478324
Prelude>
```

**Comparing Functions**

You cannot easily test functions for 'equality' within a discrete system... If I understand the constraints of 'equality' in its most general case correctly: given two functions, they must have the same domain and codomain. Further, for all elements within the domain, their values must equate. Thus, if you have a large amount of potential values, it's difficult to 'test' or prove f1 == f2, since you only have a discrete amount of memory to work with?

In addition, testing for behavioural equality is theoretically undecidable (so I've heard).

```haskell
Prelude Data.List> (not (==)) == (/=)

<interactive>:110:6: error:
    • Couldn't match expected type ‘Bool’
                  with actual type ‘a0 -> a0 -> Bool’
    • Probable cause: ‘(==)’ is applied to too few arguments
      In the first argument of ‘not’, namely ‘(==)’
      In the first argument of ‘(==)’, namely ‘(not (==))’
      In the expression: (not (==)) == (/=)

<interactive>:110:15: error:
    • Couldn't match expected type ‘Bool’
                  with actual type ‘a1 -> a1 -> Bool’
    • Probable cause: ‘(/=)’ is applied to too few arguments
      In the second argument of ‘(==)’, namely ‘(/=)’
      In the expression: (not (==)) == (/=)
      In an equation for ‘it’: it = (not (==)) == (/=)
Prelude Data.List>
```

**Polymorphic chains are quite similar to built-in (packaged within the prelude) lists**

*Quick note on Lists and Cons... And some pondering!*

```haskell
Prelude> :t []
[] :: [a]
Prelude> :t (:)
(:) :: a -> [a] -> [a]
Prelude> 4 : [2]
[4,2]
Prelude> 4 : (2 : (4 : (2 : (4 : (2 : [])))))
[4,2,4,2,4,2]
Prelude> import Data.List
Prelude Data.List> nub (4 : (2 : (4 : (2 : (4 : (2 : []))))))
[4,2]
Prelude Data.List> (nub (4 : (2 : (4 : (2 : (4 : (2 : []))))))) == (4 : [2])
True
Prelude Data.List>
```

* List Cons (:) is right associative:

```haskell
Prelude Data.List> :i (:)
type [] :: * -> *
data [] a = ... | a : [a]
  	-- Defined in ‘GHC.Types’
infixr 5 :
Prelude Data.List>
```

**Some Functions That Come 'Built In' - Provided In The Prelude**

* reverse :: [a] -> [a]
* (++)    :: [a] -> [a] -> [a]
* filter  :: (a -> Bool) -> [a] -> [a]
* map     :: (a -> b) -> [a] -> [b]

```haskell
Prelude Data.List> exmp = [[1,2,3],[9,2,9],[9,9,9]] ++ [[9,9,3]]
Prelude Data.List> exmp
[[1,2,3],[9,2,9],[9,9,9],[9,9,3]]
Prelude Data.List> reverse exmp
[[9,9,3],[9,9,9],[9,2,9],[1,2,3]]
Prelude Data.List> newexmp = reverse exmp
Prelude Data.List> filter (\x -> x /= 9) (head newexmp)
[3]
Prelude Data.List> filter (\x -> x /= 9) (reverse (head newexmp))
[3]
Prelude Data.List> filter (\x -> x /= 9) (head (tail newexmp))
[]
Prelude Data.List> head (tail newexmp)
[9,9,9]
Prelude Data.List> :i filter
filter :: (a -> Bool) -> [a] -> [a] 	-- Defined in ‘GHC.List’
Prelude Data.List> :i fmap
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  ...
  	-- Defined in ‘GHC.Base’
Prelude Data.List> map (filter (\x -> x /= 9)) newexmp
[[3],[],[2],[1,2,3]]
Prelude Data.List>
```

* id :: a -> a
* const :: a -> b -> a
* (+) :: Num a => a -> a -> a
* (/) :: Fractional a => a -> a -> a
* (<=) :: Ord a => a -> a -> Bool
* show :: Show a => a -> String

*Note: During the introduction to FP taught at Uo.Edinburgh, we discussed the identity of an operator such as ```(+)``` as being zero. This isn't hugely intuitive to individuals who have mainly studied computing or computer science from a software engineering perspective rather than a mathematical perspective. Thus, when it comes to higher order functions such as ```foldr``` or ```foldl``` understanding that zero is the identity for addition is quite important, or that one is the identity for multiplication. I'm just curious if there is a function in Haskell that provides the identity for these types of numeric functions? As if anyone mentioned the term 'identity function' to me, I would think: ```f(x) = x```, where as the identity of a function or an operator is actually quite different; and as recursion and higher order functions are used so much within areas of Haskell, it would seem to me to be a good idea to make this distinction.*

<details>

<summary>40:14 into L1.3</summary>

I'm just jumping back into these Haskell lectures after having briefly reviewed notes from the first two lectures.

</details>

**Deriving Instances**

The ```deriving``` keyword for use within code blocks that define data types is useful as it may save some time by ```deriving(Eq, Show, ...)```.

*(in the notes it says classes and automagically, whereas Andres says types and automatically, maybe kind of nit picky, but for beginners conceptualising types, typeclasses, dataclasses and instances thereof can be somewhat confusing. Thus, ensuring a consistent nomenclature - even though a well-versed Haskell dev will know what you mean - is important IMO, as a beginner may become somewhat confused - I mean, I may even be confused here).*

**Pure Functions**

* Referential Transparency
* Pure Functions Have No Side Effects
* Side Effects Expressed Via IO Type

```haskell
f :: Int -> Int
g :: Int -> IO Int -- result is not an Int, but an action
```

* Signatures that contain Monadic IO types are, from what I currently understand, fairly safe (or easier to handle).
* Probably stay away from System.IO.Unsafe: unsafePerformIO for the time being...

**Lazy Evaluation**

Building a chain:

```haskell
build :: Int -> Chain Int
build n =
  if n <= 0
    then GenesisBlock
    else Block (build (n - 1)) n
```

Okay, so let's build a chain with argument ```10000000``` and 'pass the result' to hasBlockProp even 'result':

```haskell
hasBlockProp even (build 10000000)
-- let's evaluate this by hand...
hasBlockProp even
  (if 10000000 <= 0
    then GenesisBlock
    else Block (build (10000000 - 1)) 10000000
  )
-- .. =
hasBlockProp even
  (if False
    then GenesisBlock
    else Block (build(1000000 - 1)) 10000000
  )
-- .. =
hasBlockProp even
  (Block (build (10000000 - 1)) 10000000)
-- .. =
even 10000000
  || hasBlockProp even (build (10000000 - 1))
-- .. =
True
  || hasBlockProp even (build (10000000 - 1))
-- .. =
True
```

* Lazy Evaluation > Eager Evaluation

**Positive and Negative Surprises**

I'm assuming the order of evaluation for certain functions can lead to negative surprises; e.g. execution time of foldr vs foldl on an operator such as cons (:)

**Summary**

* Defining Higher Order Functions
* Polymorphism, Data Types & Pattern Matching
* Type Classes
* Lazy Evaluation
* I was surprised to hear Idris and Agda mentioned, Idris seemed pretty interesting to me when I came across a lecture on it. However, I can't imagine ever really looking at Agda since I'm not a mathematician - I guess these languages and tools deserve a mention, given that this is the first week of an.. introductory course? Seemed fairly fast paced. I was only really able to keep up due to having completed the INF1A course publicly available via the university of Edinburgh.

**References**

<a href="#1" id="1">1</a>. Russell, B. and Whitehead, A.N. <br />
Principia Mathematica Vol. I. <br />
1910.

**Footnotes**

<a href="#s1" id="s1">1</a>. We may consider using Integer which is unbounded, or depending on our model, we may even want to change the implementation such that it handles various types, perhaps it should even be polymorphic, in which case, we may consider using Num.