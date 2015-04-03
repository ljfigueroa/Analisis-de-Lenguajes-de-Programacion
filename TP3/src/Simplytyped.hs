module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)  = Lam t (conversion' (n:b) u)
conversion' b (Let x v t)  = Tlet (conversion' b v) (conversion' (x:b) t)
conversion' b (As u t)     = Tas  (conversion' b u) t  
conversion' b LUnit        = Tunit
conversion' b (LTup t u)   = Ttup (conversion' b t) (conversion' b u)
conversion' b (LFst t)     = Tfst (conversion' b t)
conversion' b (LSnd t)     = Tsnd (conversion' b t)
conversion' b LZero        = Tzero
conversion' b (LSuc t)     = Tsuc (conversion' b t)
conversion' b (LR t u v)   = Trec (conversion' b t) (conversion' b u) (conversion' b v)
  


-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Tlet u v)            = Tlet (sub i t u) (sub (i+1) t v)
sub i t (Tas u t')            = Tas  (sub i t u) t'
sub i t Tunit                 = Tunit
sub i t (Ttup u v)            = Ttup (sub i t u) (sub i t v)
sub i t (Tfst u)              = Tfst (sub i t u)
sub i t (Tsnd u)              = Tsnd (sub i t u)
sub i t Tzero                 = Tzero
sub i t (Tsuc u)              = Tsuc (sub i t u)
sub i t (Trec u v w)          = Trec (sub i t u) (sub i t v) (sub i t w)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)       = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 VUnit      -> eval e (sub 0 Tunit u)
                 VNat x     -> eval e (sub 0 x u) 
                 _          -> error "Error de tipo en run-time, verificar type checker 2"
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker 3"

eval e (Tlet (Lam s w) v)    = eval e (sub 0 (Lam s w) v)
eval e (Tlet u v)            = case eval e u of
                 VLam t y  -> eval e (Tlet (Lam t y) v)
                 _         -> error "Error en eval let" 
eval e (Tas u t)             = eval e u
eval e Tunit                 = VUnit
eval e (Ttup u t)            = let v = eval e u
                               in VTup v (eval e t)
eval e (Tfst u)              = case eval e u of
                 VTup t _  -> t
                 _         -> error "Error en eval Tfst, se esperaba una tupla"
eval e (Tsnd u)              = case eval e u of
                 VTup _ t  -> t
                 _         -> error "Error en eval Tsnd, se esperaba una tupla"

eval e (Tzero)               = VNat Tzero
eval e (Tsuc Tzero)          = VNat (Tsuc Tzero)
eval e (Tsuc u)              = case eval e u of
                 VNat t    -> VNat (Tsuc t)
                 _         -> error "Error en eval Tsuc"
eval e (Trec t1 t2 Tzero)    = eval e t1
eval e (Trec t1 t2 (Tsuc t)) = case eval e t2 of
                 VLam u t' -> eval e (((Lam u t') :@: (Trec t1 t2 t)):@: t)
                 _         -> error "Error en eval Trec suc"
eval e (Trec t1 t2 t3)       = case eval e t3 of
                 VNat u    -> eval e (Trec t1 t2 u)
                 _         -> error "Error en eval t3 no es natural"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)  = Lam t f
quote VUnit       = Tunit
quote (VTup v v') = Ttup (quote v) (quote v')
quote (VNat t)    = t

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."




notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

nottupError :: Type -> Either String Type
nottupError t1 = err $ render (printType t1) ++ "no es una tupla "

notnatError :: Type -> Either String Type
notnatError t1 = err $ render (printType t1) ++ "no es un natural "

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1) 
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (Tlet t u) = infer' c e t >>= \tu ->
                          infer' (tu:c) e u
infer' c e (Tas u t)  = infer' c e u >>= \tu ->
                        if t == tu then ret tu else  matchError t tu
infer' c e Tunit      = ret Unit
infer' c e (Ttup t u) = infer' c e t >>= \tt ->
                        infer' c e u >>= \tu -> ret (Tup tt tu)
infer' c e (Tfst t)   = infer' c e t >>= \tt -> case tt of 
                        Tup u _ -> ret u
                        _       -> nottupError tt
infer' c e (Tsnd t)   = infer' c e t >>= \tt -> case tt of 
                        Tup _ u -> ret u
                        _       -> nottupError tt
infer' c e Tzero      = ret NatType
infer' c e (Tsuc t)   = infer' c e t >>= \tt -> case tt of
                        NatType -> ret NatType
                        _       -> notnatError tt

infer' c e (Trec t1 t2 t3) = infer' c e t1 >>= \tt ->
                             infer' c e t2 >>= \tu -> 
                             infer' c e t3 >>= \tw -> case tu of
                        Fun t u          -> if t == tt && u == Fun NatType tt
                                            then ret tt
                                            else matchError (Fun tt (Fun NatType tt)) tu
                        _                -> notfunError tu


----------------------------------
