# haskell-notes | README.md

*These are my own notes, so take them for what they are. For instance, there may be some misleading details if I have misunderstood something; as far as learning materials go, these notes are here mainly as a source of 'self-help' - I tend to learn best through writing (or typing) things down and following exercises.*

**Haskell and Cryptocurrencies Notes**

Course offered by IOG, you can find accompanying information within the builders / pioneers announcements @ the IOG discord technical server.

<details>

<summary>Questions Extracted From Notes</summary>

**Welcome Lecture**

Question: would it be accurate to say that the definition of 'hiding' (for hash functions) can be phrased such that: given a hash function H, an inverse function H' does not exist. Thus, given $h(x) = y$, $h^{-1}(y) = x$ does not exist?

Question: Given the statement regarding hash functions: "the input are arbitrary byte strings, so there are potentially infinitely many of those" - given that memory is discrete, how is it possible for there to be infinitely many input byte strings? Are we discussing hash functions in terms of their implementation on a discrete system, or their mathematical properties within 'the real world' / continuous space?

Question (this was discussed within the IOG Discord, but I don't think consensus was necessarily reached on the outcome of the question, so I'll raise it here): if one was to push two transactions to chain with the intent to have them validated within the same block, is there anything stopping the outputs from one transaction acting as the inputs to the other? Emphasis being on: both transactions are not yet validated, but have been pushed to chain (or an SPO mempool) in an ordered fashion, such that the first transaction is pushed first and the second has been pushed second; can the second transaction use outputs from the first transaction whilst both being validated within the same block? My apologies if there is an obvious answer or is this is a silly question. I would actually be interested in knowing if this is possible in both accounting based models and UTxO based models.

**An Overview Of Haskell**

Having unpaused the video and read some more about currying - probably spending too much time on this, but I am pretty sure I understand what is going on, but I suppose my question really would be: what would be a concise and intuitive definition of the concept of currying within Haskell?

**Higher Order Functions**

Do you want to maintain data structures and their respective content as 'abstract' as possible before performing any kind of evaluation in order to, for instance, maintain as much precision as possible, or is this a non-issue?

**Week Two**

No Questions - the only comment I would have is that some of the videos do seem to skip quite quickly.

</details>

**Notes**

1. [Welcome Lecture, Blockchains, Cryptocurrencies, Beyond Generic Blockchains, Ideals, Hashing Functions, Transactional Ordering, Digital Signatures, Forking, Consensus Protocols, Consensus Properties (Termination, Agreement, Strong Validity), Transactions, Blocks, Fees, IOHK, Haskell](010-Welcome.md)
2. [An overview of Haskell, Goals, History, Features, Algebraic Data Types, Type Inference, Type Classes, Data Constructors, Type Constructors, Polymorphism and Parametric Polymorphism, Referential Transparency, Eval, Functions, Nomenclature, Examples, Pattern Matching, Equational Reasoning, Currying, Symbolic Operators and Identifiers, Associativity of Infix Operators, Coding Examples (GHCi)](011-An-Overview-of-Haskell.md)
3. [Higher Order Functions, Alternative Implementations, Type-Directed Programming, More On Type Inference, Deriving Instances, Pure Functions, Lazy Evaluation, Summary](012-An-Overview-Of-Haskell.md)
4. [Basics: Prelude, Data Structure Composition: Catamorphism, Modules / Dependencies: Hackage, Documentation & Haddock, Booleans (Data Type, Constructors, Examples), Pattern Matching, Typed Holes, Some More On Functions, Guards](020-Datatypes-Functions.md)
5. [Maybe as a type constructor, Maybe as parameterised by a polymorphic type variable, Return in Haskell (Nothing), Functions Using Maybe, Pairs, Currying, Uncurrying](021-Maybe-And-Pairs.md)
6. [Cons, Breaking Lists Down With Cons, List Constructors, Functions On Lists, Either, Elem, Append, Reverse, Filter](022-Lists.md)
7. Working Slowly (But Surely).

**Code Examples**

1. [Basic Propositional Logic Implementation (prop.hs)](code_examples/prop.hs)
2. [Boilerplate Code For GHCi | Week One (week1.hs)](code_examples/week1.hs)
3. [Attempt #1 @ week1 exercise, TODO: come back to this](code_examples/intro.hs)

**Authors Note**

Having had some experience with Haskell as an undergraduate (a few years ago now), I realise (having taken the Plutus Pioneer Program) that I had only really scratched the surface. Thus, I reconsidered my goals, went back and undertook the introduction to Function Programming course provided by the University of Edinburgh (which I rate extremely highly).

*I am about to start this course, I will update this README as I make some progress through the course. As always, my thanks to the course organisers, lecturers and TAs: Lars Brünjes, Andres Löh and Alejandro García.*

<hr />

#### Other Courses I Have Undertaken (recently):

* Plutus Pioneer Program Cohort Two - officially | 2021
* Introduction to Functional Programming (INF1A) - unofficially | 2018
* Blockchains and Distributed Ledgers (BDL)<sup>1</sup> - unofficially | 2020

**Footnotes**

1. I am still working my way through these lectures and making lecture notes, as I was fortunate enough to be temporarily enrolled on that course during my time at the University of Edinburgh, thus I was able to retain the teaching materials which, unfortunately, I cannot share. I can, however, share my notes which are listed below.

**My Notes For Other Courses**

* PPP, C2: [https://github.com/jonathondilworth/detailed-plutus-lecture-notes](https://github.com/jonathondilworth/detailed-plutus-lecture-notes)
* INF1A: [https://github.com/jonathondilworth/detailed-plutus-lecture-notes/tree/main/haskell-notes](https://github.com/jonathondilworth/detailed-plutus-lecture-notes/tree/main/haskell-notes)
* BDL: [https://github.com/jonathondilworth/blockchaincourse/tree/master/My%20Notes](https://github.com/jonathondilworth/blockchaincourse/tree/master/My%20Notes)

