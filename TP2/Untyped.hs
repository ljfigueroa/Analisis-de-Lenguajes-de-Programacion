module Untyped where

import Data.List
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Common

import Prelude hiding(catch)

------------------------
-- Ejercicio 1
------------------------

num :: Integer -> LamTerm
num 0 = Abs "s" (Abs "z" (LVar "z"))
num n = let (Abs x (Abs y w)) = num (n-1)
        in (Abs "s" (Abs "z" (App (LVar "s") w)))

-----------------------
--- Sección 2 Parsers
-----------------------

totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace untyped
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          opLetter = return ' ',
                                          reservedNames = ["def"] })

 
-- Parser para comandos
parseStmt :: Parser a -> Parser (Stmt a)
parseStmt p = do
          reserved untyped "def"
          x <- identifier untyped
          reservedOp untyped "="
          t <- p
          return (Def x t)
    <|> fmap Eval p

 
parseTermStmt :: Parser (Stmt Term)
parseTermStmt = fmap (fmap conversion) (parseStmt parseLamTerm)

-- Parser para LamTerms 
parseLamTerm :: Parser LamTerm
parseLamTerm = do parseLamAbs <|> parseLamNAbs 

parseLamAbs :: Parser LamTerm
parseLamAbs = do reservedOp untyped "\\"
                 ident <- identifier untyped
                 do reservedOp untyped "."
                    s <- parseLamTerm
                    return (Abs ident s)
                  <|> do a <- many (identifier untyped)
                         reservedOp untyped "."
                         s <- parseLamTerm
                         return (Abs ident (f a s))
             
                where f [x] s = (Abs x s)
                      f (x:xs) s = (Abs x (f xs s))









parseLamNAbs :: Parser LamTerm
parseLamNAbs = do p <- many parseLamAtom
                  case p of
                   [] -> fail "Error - Empty string"
                   (x:xs) ->  do nabs' <- parseLamNAbs'
                                 return (nabs' (f p))
              
                where f [x] = x
                      f xs = (App (f (init xs)) (last xs))


parseLamNAbs' :: Parser (LamTerm -> LamTerm)
parseLamNAbs' = do napp <- parseLamNApp
                   return (\t -> (App t napp ))
                 <|> return (\t -> t)

parseLamAtom :: Parser LamTerm
parseLamAtom = do ident <- identifier untyped
                  return (LVar ident)
                <|> do reservedOp untyped "("
                       term <- parseLamTerm
                       reservedOp untyped ")"
                       return term
                     
parseLamNApp :: Parser LamTerm
parseLamNApp = do parseLamAtom <|> parseLamAbs <|> fail "Error"


-- conversion a términos localmente sin nombres
conversion  :: LamTerm -> Term 
conversion t = conv t []

-- Funciones auxiliares 
add :: String -> [(String, Int)] -> [(String, Int)]
add i [] = [(i,0)]
add i (x:xs) = let f = (fst x) /= i
               in if f then (fst x, 1+snd x):(add i xs) else ((i,0):(add' xs))

add':: [(String, Int)] -> [(String, Int)]
add' [] = []
add' (x:xs) = (fst x, 1 + snd x) :(add' xs)

lookfor :: String -> [(String, Int)] -> [Int]
lookfor i [] = []
lookfor i (x:xs) = if (fst x) == i then [snd x] else lookfor i xs

isIn :: String -> [(String, Int)] -> Bool
isIn i xs = if lookfor i xs == [] then False else True

conv :: LamTerm -> [(String, Int)] -> Term
conv (Abs i lamt) xs = Lam ((conv lamt) (add i xs))
conv (LVar v) xs = case isIn v xs of
                     True ->  Bound (head (lookfor v xs))
                     False -> Free(Global v)
conv (App l1 l2) xs = (conv l1 xs) :@: (conv l2 xs)
--

-- para testear el parser interactivamente.
testParser :: Parser LamTerm
testParser = totParser parseLamTerm                                    

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) x = f x
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: [(Name,Value)] -> Term -> Value
eval  e t = eval' t (e,[])

eval' :: Term -> (NameEnv Value,[Value]) -> Value
eval' (Bound  ii) d  = (snd d) !! ii
eval' (Free n)    d  = lookfor2 (fst d) n
eval' (t1 :@: t2) d = vapp (eval' t1 d) (eval' t2 d)
eval' (Lam t) (env,bounds) = VLam (\x -> eval' t  (env, x:bounds))





lookfor2 :: NameEnv Value -> Name -> Value
lookfor2 [x] _    = snd x
lookfor2 (x:xs) n = if fst x == n then snd x else lookfor2 xs n 


-------------------------------
-- Sección 4
-------------------------------

quote  :: Value -> Term
quote v = quote' v (Quote 0) 0


quote' :: Value -> Name -> Int -> Term
quote' (VNeutral n) name i = neutralToterm n name i

quote' (VLam f) (Quote pi) i = let qpi = (VNeutral (NFree(Quote (pi+1))))
                               in Lam (quote' (f qpi) (Quote (pi+1)) (pi+1))

neutralToterm :: Neutral -> Name -> Int -> Term
neutralToterm (NFree name) _  i = case name of
                  Quote k -> Bound (i-k)
                  Global s -> Free (Global s)
neutralToterm (NApp ne val) n i = ((neutralToterm ne n i) :@: (quote' val n i))

