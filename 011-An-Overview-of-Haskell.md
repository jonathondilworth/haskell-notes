### An Overview of Haskell

**Introduction**

Initially, we're going to touch on some elements of Plutus and how it differs from Haskell. Then, we'll briefly mention Marlowe and how I've accidentally been walking through this whole process backwards. Shortly thereafter, the goals of this lecture are available for review, with a fancy quote. Some history is fondly remembered (by some). The meat and potatoes are presently shortly thereafter (meaning, Haskell features). Right, I'm going to write the remainder out as an unordered list:

* Algebraic Data Types
* Type Inference
* Type Classes
* Data Constructors
* Type Constructors
* Polymorphism and Parametric Polymorphism
* Referential Transparency
* Evaluation
* Functions
* Nomenclature
* Examples
* Pattern Matching
* Equational Reasoning (and why it's important)
* Some More Functionz!
* Currying
* Symbolic Operators and Identifiers
* What Not To Do... (Symbolic Operator Implementation)
* Something A Little More Reasonable...
* Associativity of Infix Functions (and Operators)
* A Question About Higher Order Functions

**Let's Get To It!**

### 1. Plutus, Haskell & The Works

* Plutus is pretty much Haskell, Plutus Core is System F or a System F variant (if I understand correctly, at the very least the intermediary languages used as 'stepping stones' between the source and target are system F variant languages). But Plutus' off-chain code **IS Haskell** and Plutus' on-chain code (before it is compiled) makes heavy use of Haskell for it's *'boilerplate'* then **template Haskell for compiling to core, with some quirks**, wrapMintingPolicy seems to come to mind (which, again - I believe it has to do with the IRs for PlutusTx and the compilation pipeline from Haskell/Plutus to Plutus Core. With that said, I'm probably just an idiot who got lost and confused from trying to understand things far beyond his ability to do so! So, perhaps ignore that last part).
* Marlowe is a DSL turned Stand-alone language.

*Apparently I did this whole thing backwards. Plutus Pioneer Program probably should have come after having taken this set of lectures. Although, I didn't quite realise how little we actually learnt about Haskell within university... My apologies for wasting a potential slot. But, I'm doing my best to catch up here.*

### 2. Goals

1. What is Haskell?
2. Provide a few examples of Haskell.
3. Explain what makes Haskell unique.
4. Overwhelm us with novel, strange and quirky mathematically inspired language concepts and constructs in the hope that we don't get scared away, excellent strategy! But really, what they're trying to do is provide a brief overview of the syntax, language constructs, etc; s.t. we're not immediately scared away when (I imagine) we hear terms like "functors", "monoids" and the final boss: "Monads" (which, I have been informed on good authority, Monads was a naming mistake, they're suppose to be called: Warm Fuzzy Things [[1]](#1)).

> "If you stumble on syntax, everything else becomes difficult." <br />
> — Simon Peyton Jones

*I include quotes where I want, these are my notes, get use to it.*

### 3. What is Haskell: History

* Universities had too many variants of languages
* Wanted to 'standardise' a language
* Thus, designed by committee (thanks guys)
* Released reports from 1990 to 1998, implementations of a standard at 98/99
* Minor revision of the standard in 2010
* Some stuff has gone on which relates to Haskell, but nothing major (Idris come to mind?)
* No major changes to the core (GHC)
* GHC, Open Development Model, sounds kind of like the Linux Kernel in terms of development
* GHC perceived to be 'rock solid'

### 4. Haskell Features

* Functional
* Statically Typed
* Algebraic Data Types
* Type Inference and Polymorphism (type inference figures out a lot on its own, but I'm pretty sure being explicit is recommended, right? Even if you're using a polymorphic type, like Num, you can say: func :: Num p => p -> [p] -> p, now I can use doubles, floats, ints, integers, etc; I don't necessarily have to declare the function signature as above, because the type system can do that for me, right? But, I guess it makes it nicer to read, and how much code to we write vs read?)
* Type Classes (Ermmm, am I correct in thinking data Constructors provide a means of implementing a type class? or is that just a Data Type? I'm sure we'll learn some more about this)
* Type Classes implement parametric polymorphism (and overloading).

*Note: Some people probably wonder, I mean, I don't know who the heck would be reading my notes, but, I imagine they wonder why I even publish this stuff? Well, I guess I publish them to sort of document my journey with the intention of running it all past others when they're complete, like a peer-review process; and hopefully someone will be willing to check my work (maybe for some ADA?) and generally kind of help me? :)*

* Haskell is a 'pure' functional language, meaning referential transparency is maintained (I think?) - even when using IO, since IO is all marked as Type Classes according to Andres? Unless I've misunderstood. Maintaining referential transparency means function implementations will take on a mapping where f(x) = y, such that, f(x) will ALWAYS equal y, it is predictable (and, deterministic?) in nature, making it easy to test, reliable and safe??

* Lazy Evaluation - didn't really go into much detail here, but essentially, from what I understand already, the computer is only going to compute when it needs to.

### 4.1 Datatypes and Functions

```haskell
data Chain =
    GenesisBlock
  | Block Chain Txs

type Txs = Int
```

```haskell
type Txs = Int
data Chain = GenesisBlock
           | Block Chain Txs
           deriving(Show, Eq)
```

* Chain is a NEW data type, always use a capital letter when defining data types (functions use camel case).
* Int is a predefined data type (exists within the prelude).
* GenesisBlock and Block are data constructors which are ways of constructing values of the Chain data type.
* Data constructors can have arguments, also known as fields (as you'll need to actually implement them as functions later)
* Type synonym (Txs)
* Chain is a recursive data type, since the data constructor 'block' takes Chain as a 'field' s.t. you can repeated apply the block constructor to creating longer and longer chains.

**Example**

*I did some messing about...*

```haskell
Prelude> :{
Prelude| type Txs = Int
Prelude| data Chain = GenesisBlock
Prelude|            | Block Chain Txs
Prelude|            deriving(Show, Eq)
Prelude| :}
Prelude> :t Block
Block :: Chain -> Txs -> Chain
Prelude> :t GenesisBlock
GenesisBlock :: Chain
Prelude> -- and:
Prelude> :t Block
Block :: Chain -> Txs -> Chain
Prelude> -- chain1 = Block GenesisBlock 2 -- is valid
Prelude> chain1 = Block GenesisBlock 2
Prelude> :t chain1
chain1 :: Chain
Prelude> chain42 = Block chain1 42
Prelude> :t chain42
chain42 :: Chain
```

**Terminology (Nomenclature)**

* Bindings: the whole equation
* Left Hand Side (LHS): everything on the LHS of the equals...
* Right Hand Side (RHS): on the right of the equals...
* Expressions: what actually occurs on the RHS, so like when you show the data structure as above, you can see the set of nested functions. You can 'Show' these, you can 'Evaluate' these (eval), etc.
* Sub-Expressions: Expressions are made up of Expressions themselves.
* Prime Naming Convention, typically variants (of something) the same expression, which you can actually test to see if they evaluate to the same thing, I believe this is encouraged, right?

**Example:**

```haskell
Prelude> :{
Prelude| type Txs = Int
Prelude| data Chain = GenesisBlock
Prelude|            | Block Chain Txs
Prelude|            deriving(Show, Eq)
Prelude| :}
Prelude> chain1 = Block GenesisBlock 2
Prelude> chain42 = Block chain1 42
Prelude> chain42' = Block (Block GenesisBlock 2) 42
Prelude> chain42 == chain42'
True
Prelude>
```

**Functions in Haskell, Determine the length of OUR block chain:**

```
Prelude> :{
Prelude| chainLength :: Chain -> Int
Prelude| chainLength GenesisBlock    =    0
Prelude| chainLength (Block c _)     =    chainLength c + 1
Prelude| :}
Prelude> chainLength chain42'
2
Prelude> chainLength chain42 == chainLength chain42'
True
Prelude> chainLength chain1  == chainLength chain42'
False
Prelude>
```

* chainLength is of **type**: Chain -> Int.
* Optional **type signature**: Haskell is quite nice insofar as the type system is concerned. Having **type inference** allows developers to almost 'cheat' a little bit and 'ask' the computer / the GHC, what type do I need to be using here to get this to compile?! When something compiles in Haskell, it's syntactically and semantically correct; and it USUALLY does what you want it to do, right? Because the type system is just so strict, it's kind of difficult to get it to do something you don't want it to do? It just simply won't compile most of the time, unless you use some really unsafe operations, like unsafePerformIO.
* Anyway, it is VERY MUCH **encouraged to include (explicitly) type signatures** - as I mentioned in Haskell Features above, we all read more code than we write, so it's a nice addition to have insofar as documenting your code is concerned. I suppose you can kind of think of it like a Doc Comment, which, really, it's quite nice isn't it? I never thought I would be saying this about Haskell.
* You can have more than one '**case**' for a **function definition**, esp. since **recursion** is so prolific in Haskell. Very often you'll be using your data constructors as arguments / expressions (apparently the word is: patterns) to your functions which operate on the data structure they're evaluating.
* "The patterns are saying: depending on what this chain is, what are we going to do with it?"
* RHS: we have expressions.
* RHS of ```chainLength (Block c _)``` provides the recursive call: ```chainLength c + 1``` and since we've already defined ```chainLength GenesisBlock = 0``` we'll just keep adding 1 for each block within our data structure until we reach our GenesisBlock. This is why defining the correct cases is actually quite important. In fact, I'm pretty sure you could compile a recursive call that has no base case or terminal case, in which case, well, you're going to run out of memory? Unless, can the compiler catch this?
* Function applications binds strongly, thus chainLength c is executed before + 1, which, if I'm honest, I don't really understand why you can do this without the use brackets, since (+) is actually function too?

```haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

* Anyway, ```+ 1``` is binding more weakly than ```chainLength c```.

*Note: Oh, I didn't realise this was a course ran interactively. I've paused the video, as I tend to do as I work through these courses....*

We've been asked: what would happen if the expression: ```chainLength c + 1``` was changed to ```chainLength c + 2```.

**My intuition (and answer) would be that:** since this is a recursive function which is essentially just evaluating each block until it reaches the genesis block, it is simply a counting function (it is important to recognise the fact that we are using addition within this function all too, I will explain).

So, instead of counting in terms of ```1```, we're now counting in terms of ```2```, thus any prior chainLength would be doubled. Interesting enough, as this is essentially just a summation and the primary function of a summation is addition (whose identity is zero), the terminating case (or base case) for this call must therefore be zero. 

Similarly, if we were performing some kind of multiplication operation (doubling our length for each block, say), we would have to change the terminating case to be 1 instead of 0, since the identity of multiplication is 1 (Reasoning: anything multiplied by zero is zero).

Somewhat long-winded answer, and I'm going to feel like an absolute fool if I'm wrong.

*Unpause video.*

**Well, thank god for that.**

### 5. Pattern Matching

*I was always kind of confused as to what exactly pattern matching was...*

GHC is essentially identifying patterns (which I suppose are composite functions) and using this pattern matching 'concept' to replace the LHS with the RHS until the RHS is satisfied. Thus, you end up getting a call stack which it can make its way through? Computing step, by step.

**Equational Reasoning**

The example from the lecture:

```
  chainLength chain2
= chainLength (Block chain1 4)
= chainLength chain1 + 1
= chainLength (Block GenesisBlock 2) + 1
= (chainLength GenesisBlock + 1) + 1
= (0 + 1) + 1
= 1 + 1
= 2
```

An important way to think about functional equivalence, is one program equal to another program?

**Some More On Functions: Testing For A Particular Block**

```haskell
Prelude> :{
Prelude| hasBlock x GenesisBlock = False
Prelude| hasBlock x (Block c t) =
Prelude|   x == t || hasBlock x c
Prelude| :}
Prelude> -- GenesisBlock is a data cons with signature :: Chain
Prelude> -- it looks like x should be of type Txs to be semantically correct
Prelude> -- at least, that is how it looks to me...
Prelude> -- thus, the signature should be...
Prelude> -- hasBlock :: Txs -> Chain -> Bool
Prelude> -- GHCi will have already determined the signature, let's check...
Prelude> :t hasBlock
hasBlock :: Txs -> Chain -> Bool
Prelude> -- HURRAY! I'm so proud of myself (:
Prelude> -- too bad I didn't use a block comment, this code is going to
Prelude> -- get ripped to shreds in review.
Prelude> -- Oh, wow, I didn't know you could infer types from composite calls:
Prelude> :t chainLength chain42'
chainLength chain42' :: Int
Prelude> -- should probably make a note that these 'arguments' are known
Prelude> -- as "formal parameters"
```

### 6. Currying

*Author: this is what I thought currying is/was: Outputs from one function are inputs to another?*

I was initially finding it a little difficult to contrast my assumed notion of what currying was to the 'definition' as provided within the lecture (which could maybe be explained just a tad better, please do bear in mind I am a moron and I learn at a snails pace, I am likely of low aptitude, but I refuse to give up).

Looking at the example within the lecture (and spending some more time reading):

```haskell
hasBlock              ::      Txs -> (Chain -> Bool)
hasBlock 4            ::              Chain -> Bool
(hasBlock 4) chain 2  ::                       Bool
```

I do understand the concept (famous last words?). Since a function in Haskell (at its most base form) only accepts one argument, all functions are essentially curried. 'Currying' would appear to me to be a rather Haskell-esque word to convey a language property which describes functional composition. Since there is a relationship between functional composition and their type signatures, we also consider type signatures when currying.

*Question: would a fair definition of currying be: given a function, it is possible to transform it into a set of functions, where the output of one is another function and becomes the input of another, until the set of functions provides the same implementation/output as the original function? OR: is the original function literally an ordered set of functions: F, such that for all elements in F, each element f only takes one argument and returns another element f? Having unpaused the video and read some more about currying - probably spending too much time on this, but I am pretty sure I understand what is going on, but I suppose my question really would be: what would be a concise and intuitive definition of the concept of currying within Haskell? A further question would be: do concise and intuitive definitions of exist within the world of Haskell? That last one is tongue-in-cheek.*

### 7. A Quick Note On Operators

Operators are just identifiers. But what does this mean?

An operator is a function which can be represented by an identifier, where an identifier is either a Haskell-esque naming convention (such as the the use of prime ```'``` in equivalent functions) or a symbol, known as an **operator symbol**.

<details>

<summary>I Thought Operators Were Constructors?</summary>

Naturally, (before going and doing some reading) I was asking myself: what is an identifier? I thought operators were actually data constructors, for example, if you were to implement propositional logic as a data type, surly ```(&&)``` and ```(||)``` and ```not``` would be data constructors which returned a type of Proposition? Example:

```haskell
type Name = String
data Propositional = Var Name
                   | T
                   | F
                   | Not Propositional
                   | Propositional :||: Propositional
                   | Propositional :&&: Propositional
                   deriving (Eq, Show)
                   
-- here be ye implementation!
-- see: code_examples for an actual implementation.
-- Implementation from my other Haskell notes which reference INF1A, University of Edinburgh
```

***Just FYI: I have only done a couple of short Haskell courses for beginners, I am no where near as comfortable with Haskell as I would be with many other imperative languages. I am, however, beginning to enjoy this, I am not trying to look clever (in fact, I usually try and laugh at myself because I'm likely wrong > 51% of the time), I'm just being inquisitive.***

</details>

**Remember:** be careful about using infix and prefix notion styles when writing code that is to be read by others. Further it's probably not a great idea to start defining new and strange operator symbols for functions which may make things confusing. In short, it's always a good idea to follow an agreed upon convention. Usually there is also a set of adopted conventions for any given language. Haskell kind of rocks the boat with this one to be honest, but we do have our own conventions and it is a good idea to follow them.

**Examples:**

1. (x:xs)
2. exp == exp'
3. short formal parameter names (usually: x, n, or similar)
4. explicit function declarations are encouraged (func :: Num a => a -> a -> a)
5. etc

**GHCi: Info**

Great to know about the info command. GHCi info command ```:i```

```haskell
Prelude> :i (||)
(||) :: Bool -> Bool -> Bool 	-- Defined in ‘GHC.Classes’
infixr 2 ||
Prelude> -- infix right with priority 2 (binding priority)
```

**A Short Tutorial: What Not To Do.**

```haskell
Prelude> :{
Prelude| (|-|<<<<-<<<|-|<<<-<<<|-|>) :: Chain -> Txs -> Chain
Prelude| (|-|<<<<-<<<|-|<<<-<<<|-|>) = Block
Prelude| infixl 5 |-|<<<<-<<<|-|<<<-<<<|-|>
Prelude| :}
Prelude> chain42'' = (|-|<<<<-<<<|-|<<<-<<<|-|>) chain1 42
Prelude> chain42'' == chain42'
True
Prelude> chain42'' == chain1
False
Prelude>
```
**Something More Reasonable.**

```haskell
Prelude> :{
Prelude| (!>) :: Chain -> Txs -> Chain
Prelude| (!>) = Block
Prelude| infixl 5 !>
Prelude| :}
Prelude> chain42''' = (!>) chain1 42
Prelude> chain42''' == chain42'
True
Prelude> chain42''' == chain1
False
Prelude> chain42'''' = chain1 !> 42
Prelude> chain42'''' == chain42'
True
Prelude> chain42'''' == chain1
False
Prelude>
```

Honestly, I'm not sure what Haskell people think about this, I mean, is it reasonable? It is much more readable than the previous example (obviously), and a lot of Haskell folk are mathematicians, so perhaps they have an affinity for symbolic operators, whereas I would probably prefer a descriptive word as a function. Nevertheless, this version of the code is much more readable and thus much easier to use infix too.

**Left vs Right Associativity**

Why is this important?

Well (as far as I understand), for GHC to determine the order of evaluation between multiple functions, in some instances you will need to include brackets. However, for infix function calls, you can essentially chain them together if they have a left or right associative property. Furthermore, the evaluation order can be determined at compile time. Thus, there is no requirement to include brackets explicitly.

In addition, left and right associativity does produce different results for different functions, and the associative property matters in this respect. For example (from the lecture), we can see that if a minus function: 5 - 2 - 2 would usually equal 1 (given that (-) is ```infixl```). However, if we were to define our own infix function, say ```(!*)``` as ```infixr 6 !*``` and equate it to ```(-)```, but reverse its associative property ```infixr 6 !*```, we see the following:

```haskell
Prelude> :{
Prelude| (!*) :: Num p => p -> p -> p
Prelude| (!*) = (-)
Prelude| infixr 6 !*
Prelude| :}
Prelude> 5 !* 2 !* 2
5
Prelude> 5 - 2 - 2
1
Prelude>
```

Interestingly enough, when you apply these functions prefix and define your own ordering of evaluation, you can actually have ```(-)``` and ```(!*)``` evaluate to the same value, using the **(seemingly)** same ordering. 

```haskell
Prelude> (!*) ((!*) 5 2) 2
1
Prelude> 5 - 2 - 2
1
```

I would like to add that there are some infix functions where left and right associativity make no difference in regards to the result of the evaluation. For example, addition:

```haskell
Prelude> :{
Prelude| (+!) :: Num p => p -> p -> p
Prelude| (+!) = (+)
Prelude| infixr 6 +!
Prelude| :}
Prelude> 5 +! 5 +! 17
27
Prelude> 5 + 5 + 17
27
```

*Question: Does the associativity property of an infix function matter when using higher order functions such as ```foldl``` and ```foldr```?*


**References**

<a href="#1" id="1">1</a>. Prof Wadler. P <br />
IO and Monads. Lectures 18 - 19. <br />
Introduction to Computation (and Functional Programming) - INF1A <br />
Available at: [https://media.ed.ac.uk/playlist/dedicated/179956591/1_omcw93lf/1_ir52tuj6](https://media.ed.ac.uk/playlist/dedicated/179956591/1_omcw93lf/1_ir52tuj6) (timestamp: 00:15)