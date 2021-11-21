### An Overview of Haskell

* Marlowe is a DSL turned Standalone language.
* Plutus is pretty much Haskell, Plutus Core is System F or a System F variant, right? But Plutus' off-chain code IS Haskell and Plutus' on-chain code (before it is compiled) makes heavy use of Haskell for it's 'boilerplate' then template Haskell for compiling to core, with some quirks, wrapMintingPolicy seems to come to mind (which, again - I believe it has to do with the IRs for PlutusTx and the compilation pipeline from Haskell/Plutus to Plutus Core; I'm probably just an idiot who got lost and confused from trying to understand things far beyond my ability to do so, so perhaps ignore that last part).
* Apparently I did this whole thing backwards. Plutus Pioneer Program probably should have come after having taken this set of lectures. Although, I didn't quite realise how little we actually learnt about Haskell within university... My apologies for wasting a potential slot. But, I'm doing my best to catch up here.

**Goals**

1. What is Haskell?
2. Provide a few examples of Haskell.
3. Explain what makes Haskell unique.
4. Overwhelm us with novel, strange and quirky mathematically inspired language concepts and constructs in the hope that we don't get scared away, excellent strategy! But really, what they're trying to do is provide a brief overview of the syntax, language constructs, etc; s.t. we're not immediately scared away when (I imagine) we hear terms like "functors", "monoids" and the final boss: "Monads" (which, I have been informed on good authority, Monads was a naming mistake, they're suppose to be called: Warm Fuzzy Things [[1]](#1)).

> "If you stumble on syntax, everything else becomes difficult." <br />
> — Simon Peyton Jones

*I include quotes where I want, these are my notes, get use to it.*

**What is Haskell: History**

* Universities had too many variants of languages
* Wanted to 'standardise' a language
* Thus, designed by committee (thanks guys)
* Released reports from 1990 to 1998, implementations of a standard at 98/99
* Minor revision of the standard in 2010
* Some stuff has gone on which relates to Haskell, but nothing major (Idris come to mind?)
* No major changes to the core (GHC)
* GHC, Open Development Model, sounds kind of like the Linux Kernel in terms of development
* GHC perceived to be 'rock solid'

**Haskell Features**

* Functional
* Statically Typed
* Algebraic Data Types
* Type Inference and Polymorphism (type inference figures out a lot on its own, but I'm pretty sure being explicit is recommended, right? Even if you're using a polymorphic type, like Num, you can say: func :: Num p => p -> [p] -> p, now I can use doubles, floats, ints, integers, etc; I don't necessarily have to declare the function signature as above, because the type system can do that for me, right? But, I guess it makes it nicer to read, and how much code to we write vs read?)
* Type Classes (Ermmm, am I correct in thinking data Constructors provide a means of implementing a type class? or is that just a Data Type? I'm sure we'll learn some more about this)
* Type Classes implement parametric polymorphism and overloading which I do have some notes on from my first time trying to learn the haskellz:

> **Polymorphism and Parametric Polymorphism** <br />
> Polymorphism, I think is like the type Num, it can take on the role of an Int, Integer, Float, Double, etc. <br />
> Parametric Polymorphism is... Essentially the same thing, right? I mean, it just predated the word 'generics' in OOP? It's a type (class) which implements the same 'functionality' when given as a parameter (to a function) such that the type can be implicitly defined within a given function. Issues are resolved at compile time?

*Note: Some people probably wonder, I mean, I don't know who the heck would be reading my notes, but, I imagine they wonder why I even publish this stuff? Well, I guess I publish them to sort of document my journey with the intention of running it all past others when they're complete, like a peer-review process; and hopefully someone will be willing to check my work (maybe for some ADA?) and generally kind of help me? :)*

* Haskell is a 'pure' functional language, meaning referential transparency is maintained (I think?) - even when using IO, since IO is all marked as Type Classes according to Andres? Unless I've misunderstood. Maintaining referential transparency means function implementations will take on a mapping where f(x) = y, such that, f(x) will ALWAYS equal y, it is predictable (and, deterministic?) in nature, making it easy to test, reliable and safe??

* Lazy Evaluation - didn't really go into much detail here, but essentially, from what I understand already, the computer is only going to compute when it needs to.

**Datatypes and Functions**

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

### Pattern Matching Explained

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

### I think I'm going to take a bit of a break...

So, I've been through all this stuff before on the Intro to FP (UoE), but what I would say is that this all feels pretty fast paced compared to INF1A (albeit, it is an undergraduate module). Even though I am just reinforcing what I've already learnt, it's helpful to get multiple perspectives and I bet I'm still wrong half the time! So, I think it's just a case of continuing to learn and really reinforce this mindset / programming paradigm, because it really is quite different and it really is quite difficult. But I feel like I am getting there now, ecp. since I've been following along in GHCi and I give these lectures the time of day they deserve such that I can really digest the content. At my current rate I could only actually do two lectures per day whilst also creating this documentation for myself.

As I said, I move at a snails pace. Right, so lecture is paused at 34:02. I'll probably context switch and finish the rest of lecture 5 of the [BDL course (INFR11144)](http://www.drps.ed.ac.uk/20-21/dpt/cxinfr11144.htm).

<hr />

**References**

<a href="#1" id="1">1</a>. Prof Wadler. P <br />
IO and Monads. Lectures 18 - 19. <br />
Introduction to Computation (and Functional Programming) - INF1A <br />
Available at: [https://media.ed.ac.uk/playlist/dedicated/179956591/1_omcw93lf/1_ir52tuj6](https://media.ed.ac.uk/playlist/dedicated/179956591/1_omcw93lf/1_ir52tuj6) (timestamp: 00:15)