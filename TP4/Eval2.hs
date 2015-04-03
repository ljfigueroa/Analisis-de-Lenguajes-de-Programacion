module Eval2 (eval) where

import AST

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Monada estado
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

instance Monad StateError where
{-    return x = StateError (\s -> case x of
                                   Nothing -> Nothing
                                   Just x -> Just (x,s))
-}
    return x = StateError (\s -> Just (x,s))
    m >>= f = StateError (\s -> let run = runStateError m s
                                in case run of
                                        Nothing -> Nothing
                                        Just (x,s') -> runStateError (f x) s')

-- Clase para representar monadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateError where
    lookfor v = StateError (\s -> Just (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Clase para representar monadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateError where
    throw = StateError (\s -> Nothing)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = case (runStateError (evalComm p) initState) of
           Nothing -> initState
           Just x  -> snd x 

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let v e) = do exp <- evalIntExp e
                        update v exp
evalComm (Seq c o) = do evalComm c
                        evalComm o
evalComm (Cond b c o) = do bexp <- evalBoolExp b
                           case bexp of
                                True -> evalComm c
                                False -> evalComm o
evalComm (While b c) = do bexp <- evalBoolExp b
                          case bexp of
                               False -> return ()
                               True -> evalComm (Seq c (While b c)) 

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const i)   = return i
evalIntExp (Var var)   = do v <- lookfor var
                            return v
evalIntExp (UMinus i)  = do x <- evalIntExp i
                            return (-x)
evalIntExp (Plus i j)  = do x <- evalIntExp i
                            y <- evalIntExp j
                            return (x+y)
evalIntExp (Minus i j) = do x <- evalIntExp i
                            y <- evalIntExp j
                            return (x-y)
evalIntExp (Times i j) = do x <- evalIntExp i
                            y <- evalIntExp j
                            return (x*y)
evalIntExp (Div i j)   = do x <- evalIntExp i
                            y <- evalIntExp j
                            if y == 0 then throw
                                      else return (div x y)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue     = return True
evalBoolExp BFalse    = return False
evalBoolExp (Eq i j)  = do x <- evalIntExp i
                           y <- evalIntExp j
                           return (x == y)
evalBoolExp (Lt i j)  = do x <- evalIntExp i
                           y <- evalIntExp j
                           return (x < y)
evalBoolExp (Gt i j)  = do x <- evalIntExp i
                           y <- evalIntExp j
                           return (x > y)
evalBoolExp (And i j) = do x <- evalBoolExp i
                           y <- evalBoolExp j
                           return (x && y)
evalBoolExp (Or i j)  = do x <- evalBoolExp i
                           y <- evalBoolExp j
                           return (x || y)
evalBoolExp (Not i)   = do x <- evalBoolExp i
                           return (not x)

