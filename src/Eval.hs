-- | Responsible for converting Term into Value
module Eval where

import qualified Value  as V
import qualified Syntax as S
import Prelude hiding (fst, snd)


-- | A function that picks from the right the ith element of the environment
proj :: V.Env -> Int -> V.Value
proj env i = case env of
    V.Emp -> error "Can't happen!!"
    V.Extend env' v -> if i == 0
                       then v
                       else if i > 0
                            then proj env' (i - 1)
                            else error "Can't happen!!"


eval :: V.Env -> S.Term -> V.Value
eval env term = case term of
    S.Var (S.Ix i) ->
        proj env i
    S.Pi base fam ->
        let vbase = eval env base in
        let cfam = V.C (V.Binder fam) env in
        V.Pi vbase cfam
    S.Sg base fam ->
        let vbase = eval env base in
        let cfam = V.C (V.Binder fam) env in
        V.Sg vbase cfam
    S.Lam binder ->
        V.Lam $ V.C (V.Binder binder) env
    S.App fn arg ->
        let vfn = eval env fn in
        let varg = eval env arg in
        app vfn varg
    S.Pair term1 term2 ->
        let value1 = eval env term1 in
        let value2 = eval env term2 in
        V.Pair value1 value2
    S.Fst pair ->
        let vpair = eval env pair in
        fst vpair
    S.Snd pair ->
        let vpair = eval env pair in
        snd vpair
    S.Bool ->
        V.Bool
    S.True ->
        V.True
    S.False ->
        V.False
    S.BoolInd _motv _tcase _fcase _scrut ->
        undefined
    
snd :: V.Value -> V.Value
snd vpair = case vpair of
    V.Pair _u v ->
        v
    V.Stuck stuck typ ->
        case typ of
            V.Sg _base (V.C (V.Binder fam) env') ->
                let u = fst vpair in
                let fiber = eval (V.Extend env' u) fam in
                V.Stuck (V.Snd stuck) fiber
            _ ->
                error "Can't Happen!!"
    _ ->
        error "Can't Happen!!"

fst :: V.Value -> V.Value
fst vpair = case vpair of
    V.Pair u _v ->
        u
    V.Stuck stuck typ ->
        case typ of
            V.Sg base (V.C (V.Binder _fam) _env) ->
                V.Stuck (V.Fst stuck) base
            _ ->
                error "Can't Happen!!"
    _ ->
        error "Can't Happen!!"


app :: V.Value -> V.Value -> V.Value
app fn varg = case fn of
    V.Lam (V.C (V.Binder term') env'') -> -- we are going to create an env with one more cell in it corresponding to the bound variable and we're going to instantiate the bound variable with the argument. i.e how to substitute the argument for the bound variable but in an efficient wa
        -- make a bigger environment
        let env' = V.Extend env'' varg in
        eval env' term'
    V.Stuck stuck typ ->
        case typ of
            V.Pi base (V.C (V.Binder fam) env') ->
                {-
                M : Pi(x:A).B[x]
                M(N) : B[N]
                -}
                let stuck' = V.App stuck varg base in 
                let fiber = eval (V.Extend env' varg) fam in
                V.Stuck stuck' fiber
            _ -> error "Can't happen" -- this can only happen if smth went wrong with another part of the program
    _ ->
        error "Can't happen!!"


{- [eval]
In English:
If we have G |- M : A
the evaluator is going to take an environment for G (gamma) together with a Term M and return a Value 
-}