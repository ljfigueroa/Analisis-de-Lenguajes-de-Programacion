module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  vs (Free (Global s)) = text s
pp ii vs (i :@: c) = sep [parensIf (isLam i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (Tlet i c) = text "let " <>
                      text (vs !! ii) <>
                      text " = " <>
                      pp (ii+1) vs i <>
                      text " in " <>
                      pp (ii+1) vs c
pp ii vs (Tas c t) = pp ii vs c <>
                     text " as " <>
                     printType t
pp ii vs Tunit      = text "unit "
pp ii vs (Ttup i c) = parens $
                      pp ii vs i <>
                      text ", " <>
                      pp ii vs c
pp ii vs (Tfst i)   = text "fst " <>
                      pp ii vs i
pp ii vs (Tsnd i)   = text "snd " <>
                      pp ii vs i
pp ii vs Tzero      = text "0 "
pp ii vs (Tsuc t)   = text "suc " <>
                      pp ii vs t
pp ii vs (Trec t i c) = text "R " <>
                        pp ii vs t <>
                        pp ii vs i <>
                        pp ii vs c
          
                            
isLam (Lam _ _) = True
isLam  _      = False
   
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType Unit         = text "Unit"
printType NatType      = text "Nat"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
printType (Tup t1 t2)  = parens $ sep [ printType t1 , 
                                        text ", ", 
                                        printType t2]

isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (Free _)          = []
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Tlet t u)        = fv t ++ fv u
fv (Tas u _)         = fv u
fv Tunit             = [] 
fv (Ttup t u)        = fv t ++ fv u
fv (Tfst u)          = fv u
fv (Tsnd u)          = fv u
fv Tzero             = [] 
fv (Tsuc u)          = fv u
fv (Trec t u v)      = fv t ++ fv u ++ fv v

  
---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

