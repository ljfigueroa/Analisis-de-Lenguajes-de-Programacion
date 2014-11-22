module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Int
lookfor v (x:xs) 
    | fst x == v =  snd x
    | otherwise = lookfor v xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v n [] = [(v,n)]
update v n (x:xs)
    | fst x == v = (v,n):xs 
    | otherwise = [x] ++ update v n xs

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm comm s = case comm of
    Skip -> s
    Let v e -> update v (evalIntExp e s) s
    Seq c1 c2 -> evalComm  c2 (evalComm c1 s)
    Cond b c1 c2 -> if (evalBoolExp b s) then evalComm c1 s else evalComm c2 s
    While b c -> if (evalBoolExp b s) then evalComm comm (evalComm c s) else s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Int
evalIntExp exp s = case exp of
    Const i -> i
    Var v -> lookfor v s
    UMinus e -> (-1) * evalIntExp e s
    Plus e1 e2 -> (+) (evalIntExp e1 s) (evalIntExp e2 s)
    Minus e1 e2 -> (-) (evalIntExp e1 s) (evalIntExp e2 s)
    Times e1 e2 -> (*) (evalIntExp e1 s) (evalIntExp e2 s)
    Div e1 e2 -> (div) (evalIntExp e1 s) (evalIntExp e2 s)
                    
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp b s = case b of
    BFalse -> False
    BTrue -> True
    Eq e1 e2 -> (==) (evalIntExp e1 s) (evalIntExp e2 s)
    Lt e1 e2 -> (<) (evalIntExp e1 s) (evalIntExp e2 s)
    Gt e1 e2 -> (>) (evalIntExp e1 s) (evalIntExp e2 s)
    And e1 e2 -> (&&) (evalBoolExp e1 s) (evalBoolExp e2 s)
    Or e1 e2 -> (||) (evalBoolExp e1 s) (evalBoolExp e2 s)
    Not e -> not (evalBoolExp e s)
