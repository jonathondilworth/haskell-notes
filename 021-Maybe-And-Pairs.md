### Optional Values

*Maybe is for optional values.*

```haskell
data Maybe a = Nothing
             | Just a
```

* Type Constructor
* Takes one Type Variable
* Other programming language functions do not always return a value (or if they do, it may be ```NULL``` or ```0```, etc):
```java
public static void myStaticFunc(String arg1) { // ... }
```
* In Haskell, the Maybe type is used when a function may need to return ```Nothing``` as functions are required to return a value.

```haskell
-- Constructors
Nothing :: Maybe a
Just    :: a -> Maybe a
```

*Question: I admit, I'm perhaps a little lost. The Nothing constructor returns ```Maybe a```..? Is this because ```Maybe a``` can take the form of ```Nothing```..?*

*Perhaps a little helpful, from the docs:*

> "The Maybe type is also a monad. It is a simple kind of error monad, where all errors are represented by Nothing." <br /> [Documentation Link](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Maybe.html#v:Nothing)


### Functions on Maybe

```haskell
func :: Maybe a -> ...
func Nothing  = ...
func (Just a) = ...
```

*I got a little lost here again, but I understand now.*

```haskell
-- A Useful Maybe Function
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _  (Just a) = a
```

This is useful because you can specify a 'default' value for the function to return (if and only if Nothing has been provided as the second formal parameter). Furthermore, if the type signature for fromMaybe is satisfied (the second formal parameter is NOT NOTHING, then simply return the second formal parameter).

In another language it may look something like this:

```php
function fromMaybe ($defaultValue, $maybeValue=NULL) {
  return $maybeValue !== NULL ? $maybeValue : $defaultValue;
}
echo (fromMaybe("DEFAULT-1", "Hello World\n"));
// prints "Hello World" to console
echo (fromMaybe("DEFAULT-2"));
// prints "DEFAULT-2" to console
```
In Haskell we follow (seemingly strange) conventions, such as naming formal parameters with a single letter. Furthermore, formal parameters for functions are often defined using the same (single letter) name as type variables.

**Remember: the function signature contains a polymorphic type variable, whilst the function definition contains a formal parameter. These are two distinct 'elements' (or language constructs) even though they're often given the same name to make the language more readable.**

### Some Example(s)

```haskell
Prelude> :{
Prelude| fromMaybe :: a -> Maybe a -> a
Prelude| fromMaybe def Nothing = def
Prelude| fromMaybe _  (Just a) = a
Prelude| :}
Prelude> fromMaybe "hello world" Nothing
"hello world"
Prelude> fromMaybe "hello world" (Just "Brave New World")
"Brave New World"
Prelude> :i Just
type Maybe :: * -> *
data Maybe a = ... | Just a
  	-- Defined in ‘GHC.Maybe’
Prelude> :{
Prelude| orelse :: Maybe a -> Maybe a -> Maybe a
Prelude| orelse Nothing e  = e
Prelude| orelse (Just o) _ = Just o
Prelude| :}
Prelude> orelse Nothing (Just "Hello")
Just "Hello"
Prelude> orelse (Just "Hello") Nothing
Just "Hello"
Prelude> orelse (Just "Hello") (Just "World")
Just "Hello"
Prelude> orelse (Just "World") (Just "Hello")
Just "World"
Prelude> Nothing `orelse` Just "World"
Just "World"
Prelude> Just "Hello" `orelse` Just "World"
Just "Hello"
Prelude> :{
Prelude| mapMaybe :: (a -> b) -> Maybe a -> Maybe b
Prelude| mapMaybe f (Just a) = Just (f a)
Prelude| mapMaybe _ Nothing  = Nothing
Prelude| :}
Prelude> -- as an example:
Prelude> :{
Prelude| headStr :: String -> Char
Prelude| headStr (x:xs) = x
Prelude| :}
Prelude> headStr "Hello"
'H'
Prelude> mapMaybe headStr (Just "Hello")
Just 'H'
Prelude> mapMaybe headStr Nothing
Nothing
Prelude> -- why not just use fmap?
Prelude> headStr <$> (Just "Hello")
Just 'H'
Prelude> headStr <$> Nothing
Nothing
Prelude> fmap headStr (Just "Hello")
Just 'H'
Prelude> fmap headStr Nothing
Nothing
Prelude>
```

*Right, so the speed at which the lecture is moving at is difficult to watch it without continuously pausing it; and more code does start to appear seemingly out of nowhere, such as ```mapMaybe``` (is the mapMaybe function required, could you use ```fmap``` or ```<$>```?)*

Okay, so having implemented mapMaybe, I wrote the following implementation of the ```addMaybe``` function (such that it can add parameters of the polymorphic type ```Num```:

```haskell
Prelude> :{
Prelude| addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
Prelude| addMaybe (Just p) (Just q)   =   Just (p + q)
Prelude| addMaybe _        _          =   Nothing
Prelude| :}
Prelude> addMaybe (Just 2.1) (Just 1.2)
Just 3.3
Prelude> addMaybe (Just 7.7) Nothing
Nothing
Prelude> addMaybe Nothing (Just 4.2)
Nothing
Prelude> addMaybe (Just 4.2) (Just 4.2)
Just 8.4
```

#### Function Mapping (With Maybe) For Two Optional Types

```haskell
Prelude> :{
Prelude| liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
Prelude| liftMaybe f (Just x) (Just y) = Just (f x y)
Prelude| liftMaybe _ _        _        = Nothing
Prelude| :}
Prelude> liftMaybe (*) (Just 42) (Just 24.72)
Just 1038.24
Prelude> liftMaybe (*) (Just 42) (Just 24)
Just 1008
Prelude> liftMaybe (++) (Just [1,2,3]) (Just [4,5,6])
Just [1,2,3,4,5,6]
Prelude> :{
Prelude| tailStr :: String -> String
Prelude| tailStr (x:xs) = xs
Prelude| :}
Prelude> tailStr "Hello"
"ello"
Prelude> :t headStr
headStr :: String -> Char
Prelude> :t tailStr
tailStr :: String -> String
Prelude> :t liftMaybe
liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
Prelude> liftMaybe (:) (Just (headStr "Hello")) (Just (tailStr "Hello"))
Just "Hello"
```

### Pairs In Haskell

```haskell
-- pair | special syntax
data (a, b) = (a, b)

-- Constructor:
(, ) :: a -> b -> (a, b)
```

A silly example:

```haskell
Prelude> :{
Prelude| pFunc :: (a, b) -> [a]
Prelude| pFunc (a, b) = [a]
Prelude| :}
Prelude> pFunc (1000, "cash")
[1000]
```
Extracting Components:

```haskell
Prelude> :{
Prelude| fc :: (a, b) -> a
Prelude| fc (a, b) = a
Prelude| sc :: (a, b) -> b
Prelude| sc (a, b) = b
Prelude| :}
Prelude> fc (42, 24)
42
Prelude> sc (42, 24)
24
Prelude>
```

**REMEMBER: Since there is only a single constructor for a Pair ```(, )``` ... This means you only need one instance of pattern matching:**

```haskell
Prelude> :{
Prelude| swap :: (a, b) -> (b, a)
Prelude| swap (a, b) = (b, a)
Prelude| :}
Prelude> swap (2, 4)
(4,2)
Prelude>
```
### Currying

```haskell
curr :: ((a, b) -> c) -> a -> b -> c
curr f a b = f (a, b)
```

**Example:**

```haskell
Prelude> :{
Prelude| curr :: ((a, b) -> c) -> a -> b -> c
Prelude| curr f a b = f (a, b)
Prelude| :}
Prelude> curr swap 4 '2'
('2',4)
Prelude> curr swap "2" '4'
('4',"2")
Prelude>
```

### Currying (Again) & It's Inverse

```haskell
Prelude> uncurr (+) (4, 2)
6
Prelude> uncurr (+) (curr swap 4 2)
6
```

*Note: in Haskell we like curry.*

### Summary

Within This Lecture, We Reviewed The Following:

* Maybe as a type constructor
* Maybe as parameterised by a polymorphic type variable
* Return in Haskell (Nothing)
* Functions Using Maybe
* Pairs
* Currying
* Uncurrying