module Value where
import Syntax (Term, Binder)


newtype VBinder = Binder Term

-- | This is supposed to be a type family
data Closure = 
    -- | Gamma, x : A |- binder : B
    --   env : for each z:C in Gamma, a value
    C VBinder Env

-- | 
data Env =
    -- | If you have no variables
    Emp
    -- | If you have n variables
    | Extend Env Value

-- | De bryne levels (similar to de bryne indices but count from the right)
newtype VarI = Lvl Int

data Value =
      Pi Value Closure
    | Sg Value Closure
    | Lam Closure
    | Bool
    | True
    | False
    | Pair Value Value
    -- | We need to store the type of the thing that is stuck (the second argument is the type)
    | Stuck Stuck Value

-- | This kind of a sub type of values
data Stuck =
      Var VarI
    | Fst Stuck
    | Snd Stuck
    | App Fn Arg Base
    | BoolInd Motive TCase FCase Scrut

type Motive = Closure
type TCase = Value
type FCase = Value
type Scrut = Stuck

type Fn = Stuck
type Arg = Value
type Base = Value


{- [Value]
This is another representation of the core syntax, also called the semantic domain
It helps with checking if two well typed terms are equal according to the rules of the type theory
In the process of converting from Term to Value we'll squeeze out all the beta reductions
- We want to make sure that nothing in the value type is reduced by a beta reduction
- We have variables since in a context you want to make sure you have variables
- This 'language' is going to treat variable a little bit differently i.e the variables are indexed from the right as opposed from the left like in the Term rep

The point of the closure is that we don't have to do any expensive substitutions
It is going to be easier to compare Values being equal compared to Terms being equal

So we need to implement a function that takes a Term and converts it to a Value
-}