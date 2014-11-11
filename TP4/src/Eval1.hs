module Eval1 (eval) where

import AST

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip = return ()
evalComm (Let v e) = update v e
evalComm (Seq c o) = do evalComm c
                        evalComm o
                        return ()
evalComm 

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
--evalIntExp = undefined
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
                            return (div x y)


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
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

