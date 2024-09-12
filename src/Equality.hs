-- | Implementing definitional Equality checker for dependent type theory

module Equality where

import qualified Value  as V
import qualified Eval
import Control.Exception (Exception, throw)


data EqualityException = Unequal
    deriving Show

instance Exception EqualityException

-- | For equating types
equateTyp :: Int -> V.Value -> V.Value -> ()
equateTyp len typ0 typ1 =
    case (typ0, typ1) of
        (V.Pi base0 (V.C (V.Binder fam0) env0), V.Pi base1 (V.C (V.Binder fam1) env1)) ->
            let _ = equateTyp len base0 base1 in
            -- the way to compare families is to instantiate them at a variable
            -- doesn't matter the base since we already checked that they are equal
            let var = V.Stuck (V.Var (V.Lvl len)) base0 in
            let fiber0 = Eval.eval (V.Extend env0 var) fam0 in
            let fiber1 = Eval.eval (V.Extend env1 var) fam1 in
            equateTyp (len + 1) fiber0 fiber1
        (V.Pi _ _, _) ->
            throw Unequal
        (V.Bool, V.Bool) ->
            ()
        (V.Bool, _) ->
            throw Unequal
        _ ->
            undefined


-- | For equating elements of types
equate :: Int -> V.Value -> V.Value -> V.Value -> ()
equate len typ val0 val1 =
    case typ of -- this is how we implement the eta laws
        V.Pi base (V.C (V.Binder fam) env) ->
            let var = V.Stuck (V.Var (V.Lvl len)) base in
            let result0 = Eval.app val0 var in
            let result1 = Eval.app val1 var in
            -- The generic fiber of the family
            let fiber = Eval.eval (V.Extend env var) fam in
            equate (len + 1) fiber result0 result1
        V.Sg base (V.C (V.Binder fam) env)->
            let fst0 = Eval.fst val0 in
            let fst1 = Eval.fst val1 in
            let _ = equate len base fst0 fst1 in
            let snd0 = Eval.snd val0 in
            let snd1 = Eval.snd val1 in
            let fiber = Eval.eval (V.Extend env fst1) fam in
            equate len fiber snd0 snd1
        -- V.Bool -> -- booleans don't have an eta law, it is prohibitively expensive
        --     undefined
        _ ->
            case (val0, val1) of
                (V.True, V.True) ->
                    ()
                (V.True, _) ->
                    throw Unequal
                (V.False, V.False) ->
                    ()
                (V.False, _) ->
                    throw Unequal
                (V.Stuck stuck0 typ0 , V.Stuck stuck1 typ1) ->
                    let _ = equateTyp len typ0 typ1 in
                    equateStuck len stuck0 stuck1
                _ ->
                    throw Unequal
  where
    equateStuck :: Int ->V.Stuck -> V.Stuck -> ()
    equateStuck len' stuck0 stuck1 = 
        case (stuck0, stuck1) of
            (V.Var (V.Lvl lvl0), V.Var (V.Lvl lvl1)) ->
                if lvl0 == lvl1
                then ()
                else throw Unequal
            (V.Fst stuck0', V.Fst stuck1') ->
                equateStuck len' stuck0' stuck1'
            (V.Snd stuck0', V.Snd stuck1') ->
                equateStuck len' stuck0' stuck1'
            (V.App fn0 arg0 base0, V.App fn1 arg1 base1) ->
                let _ = equateStuck len' fn0 fn1 in
                let _ = equateTyp len' base0 base1 in
                equate len' base0 arg0 arg1
            _ ->
                throw Unequal

{- [equate]
The Problem {G |- A == B type}

Checking whether two values denoting types are equal
conversion rule : given an element (m) of type A and and A and B are definitionally equal types then
    m has to be of type B as Well
so the conversion type says we need to know when two types are equal
in generality you'll also need to check whether elements of the type are equal

- We need to remember how long the context is hence the Int, you can also include the whole context
- You can find out everything about a sigma type by checking its first and second projection

This checker is the part of a "proof assistant" that is run alot so it is important for it to be really fast
To prove that the checker is correct we'll have to use logical relations.

-}