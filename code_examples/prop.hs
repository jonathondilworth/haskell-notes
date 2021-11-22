import Data.List

{-
  Learnt during / taken from the University of Edinburgh Lecture Series Intro to Computation and FP, INF1A
  Original Author: Prof Wadler. P
  Any weird changes / removals / mistakes, those are mine! Sorry!
  Still trying to remember how to use lookup and envs, may need to revisit the original lecture...
-}

type Name = String
data Prop = Var Name
         | F
         | T
         | Not Prop
         | Prop :|: Prop
         | Prop :&: Prop
         deriving (Eq, Show)

type Names = [Name]
type Env = [(Name, Bool)]

showProp :: Prop -> String
showProp (Var x)    =   x
showProp (F)        =   "F"
showProp (T)        =   "T"
showProp (Not p)    =   par ("~" ++ showProp p)
showProp (p :|: q)  =   par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q)  =   par (showProp p ++ "&" ++ showProp q)

par :: String -> String
par s   =   "(" ++ s ++ ")"

names :: Prop -> Names
names (Var x)       =   [x]
names (F)           =   []
names (T)           =   []
names (Not p)       =   names p
names (p :|: q)     =   nub (names p ++ names q)
names (p :&: q)     =   nub (names p ++ names q)

evalProp :: Env -> Prop -> Bool
evalProp e (Var x)  =   lookUp e x
evalProp e (F)      =   False
evalProp e (T)      =   True
evalProp e (Not p)  =   not (evalProp e p)
evalProp e (p :|: q) =  evalProp e p || evalProp e q
evalProp e (p :&: q) =  evalProp e p && evalProp e q

envs :: Names -> [Env]
envs []       =  [[]]
envs (x:xs)   =  [ (x,b):e | b <- bs, e <- envs xs ]
  where
  bs   =   [False, True]

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x   =   the [y | (x',y) <- xys, x == x']
  where
  the [x]   =   x

main :: IO ()
main = do
  
  let p1 = (Var "a" :&: Var "b")
  print (showProp p1)

  let p2 = (Var "a" :&: Var "b") :|: (Not (Var "a") :&: Not (Var "b"))
  print (showProp p2)

  print (evalProp [("a", False), ("b", False)] p2)