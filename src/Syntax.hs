{-# LANGUAGE GADTs #-}
-- | This module represents the core syntax
module Syntax where


import qualified Unbound.Generics.LocallyNameless as Unbound


-- type Ix = Int

-- | This represents a way to count de bruyne indices
newtype VarI = Ix Int
-- (* x : A, y : B, z : C |- <<<you are here>>>), so z would be the first index (which is the most recently bound variable) and counts higher as you get to
-- older variables, 

-- Binder(A) type
--   B : A ---> Binder(A)

data Binder where
    B :: a -> Binder

-- | This is an inductive type of terms
data Term =
    -- | We can have variables 
     Var VarI
     -- | And Pi types with two arguments the i.e Pi (x:A) Bx where x is the first term and Bx is the second term, the second terms also binds a variable
     --   In Ocaml (Pi of term * term binder where type 'a binder = B of 'a)
    | Pi Term Term -- Binder
    -- | Sigma types
    | Sg Term Term -- Binder
    -- | Lambda abstractions (e.g \x. Mx )
    | Lam Term -- Binder
    -- | Pairs which are elements of the Sigma types
    | Pair Term Term
    -- | We also need application with two terms (the function and the argument)
    | App Term Term
    -- | the first projection from the Pair type
    | Fst Term
    -- | The second projection from the Pair type
    | Snd Term
    -- | We also need some base cases
    | Bool
    | True
    | False
    -- | We have a case statement
    | BoolInd Motive TCase FCase Scrut

-- | The motive of the boolInduction (Case)
type Motive = Term
type TCase = Term
type FCase = Term 
-- | The thing we are doing induction on
type Scrut = Term

{-
The good representation of core syntax is de Bruyne indices.
Pi and Sg are the types but in the above representation there is no distinction between types and terms
Having both Pi and Lam corresponds to type theory where we have both the Pi and Lambda operators.
-}