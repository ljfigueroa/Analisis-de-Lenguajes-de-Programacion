module Eval3 (eval) where

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
eval :: Comm -> (ErrHandler,Int)
eval p = evalComm p (initState,0)

-- Evalua un comando en un estado dado
-- Completar definicio

data ErrHandler = ERROR | OK State deriving Show

evalComm :: Comm -> (State,Int) -> (ErrHandler,Int)
evalComm Skip (s,i) = (OK s,i)
evalComm (Let v e) (s,i) = case eval of
                            Nothing -> (ERROR, c)
                            Just x -> (OK (update v x s), c)
                        where (eval,c) = evalIntExp e (s,i)
evalComm (Seq c1 c2) s = case state of
                            ERROR -> (ERROR,c)
                            OK st -> evalComm c2 (st,c)
                        where (state,c) = evalComm c1 s
evalComm (Cond b c1 c2) (s,i) | bool == True = evalComm c1 (s,i) 
                              | otherwise    = evalComm c2 (s,i)
                              where bool = evalBoolExp b (s,i)

evalComm (While b c) (s,i) | bool == False = (OK s,i)
                           | otherwise = case eval of
                                        ERROR -> (ERROR,n)
                                        OK state -> evalComm (While b c) (state,n)
                            where bool = evalBoolExp b (s,i)
                                  (eval,n) = evalComm c (s,i)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> (State,Int) -> (Maybe Int,Int)
evalIntExp (Const i) (s,n) = (Just i,n)
evalIntExp (Var v) (s,n) = (Just (lookfor v s),n)
evalIntExp (UMinus e) (s,n) = case eval of
                            Nothing -> (Nothing,i+n)
                            Just x -> (Just ((-1) * x),i+n+1)
                        where (eval,i) = evalIntExp e (s,0)

evalIntExp (Plus e1 e2) (s,n) = case eval1 of
                                Nothing -> (Nothing,i1+n)
                                Just x -> case eval2 of
                                            Nothing -> (Nothing,i1+i2+n)
                                            Just y -> (Just ((+) x y),i1+i2+n+1)
                        where (eval1,i1) = evalIntExp e1 (s,0)
                              (eval2,i2) = evalIntExp e2 (s,0)
evalIntExp (Minus e1 e2) (s,n) = case eval1 of 
                                    Nothing -> (Nothing,i1+n)
                                    Just x -> case eval2 of
                                                Nothing -> (Nothing,i1+i2+n)
                                                Just y -> (Just ((-) x y),i1+i2+n+1)
                        where (eval1,i1) = evalIntExp e1 (s,0)
                              (eval2,i2) = evalIntExp e2 (s,0)
evalIntExp (Times e1 e2) (s,n) = case eval1 of
                                    Nothing -> (Nothing,i1+n)
                                    Just x -> case eval2 of
                                                Nothing -> (Nothing,i1+i2+n)
                                                Just y -> (Just ((*) x y),i1+i2+n+1)
                        where (eval1,i1) = evalIntExp e1 (s,0)
                              (eval2,i2) = evalIntExp e2 (s,0)
evalIntExp (Div e1 e2) (s,n) = case eval1 of
                                Nothing -> (Nothing,i1+n)
                                Just x -> case eval2 of
                                            Nothing -> (Nothing,i1+i2+n)
                                            Just 0 -> (Nothing,i1+i2)
                                            Just y -> (Just ((div) x y),i1+i2+n+1)
                        where (eval1,i1) =  evalIntExp e1 (s,0)
                              (eval2,i2) =  evalIntExp e2 (s,0)
                   
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> (State,Int) -> Bool
evalBoolExp b s = case b of
    BFalse -> False
    BTrue -> True
    Eq e1 e2 -> (==) (evalIntExp e1 s) (evalIntExp e2 s)
    Lt e1 e2 -> (<) (evalIntExp e1 s) (evalIntExp e2 s)
    Gt e1 e2 -> (>) (evalIntExp e1 s) (evalIntExp e2 s)
    And e1 e2 -> (&&) (evalBoolExp e1 s) (evalBoolExp e2 s)
    Or e1 e2 -> (||) (evalBoolExp e1 s) (evalBoolExp e2 s)
    Not e -> not (evalBoolExp e s)

