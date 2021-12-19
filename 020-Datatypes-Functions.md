### Week Two (2.1): Datatypes & Functions

**Introduction**

Topics Covered In Week Two:

* Bool
* Maybe
* Lists
* Binary Trees
* Others

**Catamorphism**

Standard Design Pattern: **Catamorphism** *(Top-Down Transformation)* - how the shape of the data type determines the shape of the function.

Bool is packaged within the **Prelude**.

Module Documentation: Hackage.

* For all data types, definitions are visible.
* For all functions, signatures are visible.
* General description provided.
* Source code linked to entry.

*Some information about Haddock.*

* Doc command example: ```:doc any``` (displays the description text).

### Info: Booleans

Fairly intuitive, a datatype with two constructors.

```haskell
-- datatype
data Bool = False
          | True
          deriving (Show)

-- constructors
False :: Bool
True  :: Bool
```

A quick example / messing about:

```haskell
func  :: Bool -> Bool
func False = True
func True  = False

func' :: Bool -> Bool
func' x = not x

func True  == func' True
func False == func' False
```

#### More Info: Pattern Matching

* The order of patterns matter (AFAIK, in a recursive function call, for instance, you always start with the terminating case - or base case? This is perhaps more intuitive to understand if one considers the ordering).
* The following is taken directly from the slides:
* Patterns are matched in order.
* In particular catch-all cases must come last.
* **Best practice: split programming problems by expanding variables into all possible constructors for the type of the variable.**
* For non-overlapping cases the order does not matter.

### Back To Booleans

The ordering of pattern matching does not matter for Bools, since only True can match, or only False can match.

**Nomenclature: Typed Holes**

```haskell
Prelude> -- Typed Holes
Prelude> :{
Prelude| nt :: Bool -> Bool
Prelude| nt = _
Prelude| :}

<interactive>:27:6: error:
    • Found hole: _ :: Bool -> Bool
    • In the expression: _
      In an equation for ‘nt’: nt = _
    • Relevant bindings include
        nt :: Bool -> Bool (bound at <interactive>:27:1)
      Valid hole fits include
        nt :: Bool -> Bool (bound at <interactive>:27:1)
        nnnot :: Bool -> Bool (defined at <interactive>:15:1)
        not :: Bool -> Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Classes’))
        id :: forall a. a -> a
          with id @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        pred :: forall a. Enum a => a -> a
          with pred @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
        succ :: forall a. Enum a => a -> a
          with succ @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
Prelude> -- introduce a formal parameter
Prelude> :{
Prelude| nt :: Bool -> Bool
Prelude| nt x = _
Prelude| :}

<interactive>:32:8: error:
    • Found hole: _ :: Bool
    • In the expression: _
      In an equation for ‘nt’: nt x = _
    • Relevant bindings include
        x :: Bool (bound at <interactive>:32:4)
        nt :: Bool -> Bool (bound at <interactive>:32:1)
      Valid hole fits include
        it :: Bool (defined at <interactive>:18:1)
        x :: Bool (bound at <interactive>:32:4)
        otherwise :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        False :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        True :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        maxBound :: forall a. Bounded a => a
          with maxBound @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
        (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
Prelude> -- you could write nt x = not x, but we're disallowing that solution for obvious reasons
Prelude> -- so: exhaustive pattern matching?
Prelude> :{
Prelude| nt :: Bool -> Bool
Prelude| nt True = _
Prelude| nt False = _
Prelude| :}

<interactive>:38:11: error:
    • Found hole: _ :: Bool
    • In the expression: _
      In an equation for ‘nt’: nt True = _
    • Relevant bindings include
        nt :: Bool -> Bool (bound at <interactive>:38:1)
      Valid hole fits include
        it :: Bool (defined at <interactive>:18:1)
        otherwise :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        False :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        True :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        maxBound :: forall a. Bounded a => a
          with maxBound @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
        minBound :: forall a. Bounded a => a
          with minBound @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))

<interactive>:39:12: error:
    • Found hole: _ :: Bool
    • In the expression: _
      In an equation for ‘nt’: nt False = _
    • Relevant bindings include
        nt :: Bool -> Bool (bound at <interactive>:38:1)
      Valid hole fits include
        it :: Bool (defined at <interactive>:18:1)
        otherwise :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
        False :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        True :: Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Types’))
        maxBound :: forall a. Bounded a => a
          with maxBound @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
        minBound :: forall a. Bounded a => a
          with minBound @Bool
          (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
Prelude> -- now two holes, the answer is obvious, but the process is what may be helpful in the future
Prelude> :{
Prelude| nt :: Bool -> Bool
Prelude| nt False = True
Prelude| nt True = False
Prelude| :}
Prelude> -- always good to play
Prelude> :{
Prelude| nt True == not True
Prelude| :}
True
Prelude> :{
Prelude| nt False == not False
Prelude| :}
True
Prelude>
```
*Some Boolean Logic, Propositions & More Info On Infix:*

```haskell
Prelude> :{
Prelude| or :: Bool -> Bool -> Bool
Prelude| or True _ = True
Prelude| or False b = b
Prelude| :}
Prelude> True `or` True
True
Prelude> True `or` False
True
Prelude> False `or` True
True
Prelude> False `or` False
False
Prelude> True || True
True
Prelude> True || False
True
Prelude> False || True
True
Prelude> False || False
False
Prelude>
```

Although this is an elementary example, you would likely want to write test cases to compare ```or``` with ```||``` (for example).

#### ifthenelse (example function)

```haskell
Prelude> :{
Prelude| ifthenelse :: Bool -> a -> a -> a
Prelude| ifthenelse False t e = e
Prelude| ifthenelse True  t e = t
Prelude| :}
Prelude> -- if: False, return ELSE -- if: True, return THEN, basically.
Prelude> ifthenelse True (2 :: Int) (3 :: Int)
2
Prelude> ifthenelse False (2 :: Int) (3 :: Int)
3
Prelude>
```
*Note: the function uses the polymorphic type ```a``` - ensure it remains consistent (obviously) otherwise the compiler will get angry with you, as it should.*

**You always need else.**

### Guards

```haskell
Prelude> :{
Prelude| ifThenElse :: Bool -> a -> a -> a
Prelude| ifThenElse b t e
Prelude|   |  b         =  t
Prelude|   |  otherwise =  e
Prelude| :}
Prelude> ifThenElse True (2 :: Int) (42 :: Int) == ifthenelse True (2 :: Int) (42 :: Int)
True
Prelude>
```

Every condition (LHS after guard) are of type Bool. Tried one at a time until a match is found (True).

*Note: typically use otherwise in guards, readable.*

**Summary**

Within part one of week two, the following content was covered:

* Basics: Prelude
* Data Structure Composition: Catamorphism
* Modules / Dependencies: Hackage
* Documentation & Haddock
* Booleans (Data Type, Constructors, Examples)
* Pattern Matching
* Typed Holes
* Some More On Functions
* Guards