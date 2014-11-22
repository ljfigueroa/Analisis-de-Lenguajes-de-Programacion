module Eval2 (eval) where

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
eval :: Comm -> ErrHandler
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicio

data ErrHandler = ERROR | OK State deriving Show

evalComm :: Comm -> State -> ErrHandler
evalComm Skip s = OK s
evalComm (Let v e) s = case eval of
                        Nothing -> ERROR
                        Just x -> OK (update v x s)
                     where eval = evalIntExp e s
evalComm (Seq c1 c2) s = case state of
                            ERROR -> ERROR
                            OK st -> evalComm c2 st
                        where state = evalComm c1 s
evalComm (Cond b c1 c2) s | bool == True = evalComm c1 s 
                          | otherwise    = evalComm c2 s
                          where bool = evalBoolExp b s

evalComm (While b c) s 
    | bool == False = OK s
    | otherwise = case eval of
                    ERROR -> ERROR
                    OK state -> evalComm (While b c) state
    where bool = evalBoolExp b s
          eval = evalComm c s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Maybe Int
evalIntExp (Const i) s = Just i
evalIntExp (Var v) s = Just (lookfor v s)
evalIntExp (UMinus e) s = case eval of
                            Nothing -> Nothing
                            Just x -> Just ((-1) * x)
                        where eval = evalIntExp e s

evalIntExp (Plus e1 e2) s = case eval1 of
                            Nothing -> Nothing
                            Just x -> case eval2 of
                                Nothing -> Nothing
                                Just y -> Just ((+) x y)
                        where eval1 =  evalIntExp e1 s
                              eval2 =  evalIntExp e2 s
evalIntExp (Minus e1 e2) s = case eval1 of 
                            Nothing -> Nothing
                            Just x -> case eval2 of
                                Nothing -> Nothing
                                Just y -> Just ((-) x y)
                        where eval1 =  evalIntExp e1 s
                              eval2 =  evalIntExp e2 s
evalIntExp (Times e1 e2) s = case eval1 of
                            Nothing -> Nothing
                            Just x -> case eval2 of
                                Nothing -> Nothing
                                Just y -> Just ((*) x y)
                        where eval1 =  evalIntExp e1 s
                              eval2 =  evalIntExp e2 s
evalIntExp (Div e1 e2) s = case eval1 of
                            Nothing -> Nothing
                            Just x -> case eval2 of
                                Nothing -> Nothing
                                Just 0 -> Nothing
                                Just y -> Just ((div) x y)
                        where eval1 =  evalIntExp e1 s
                              eval2 =  evalIntExp e2 s
                   
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
