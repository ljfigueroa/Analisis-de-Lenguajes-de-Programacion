module Eval3 (eval) where

import AST

-- Estados
type Env = [(Variable,Int)]

-- Monada estado
newtype StateErrorTick a =  StateErrorTick { runStateErrorTick :: (Int,Env) -> Maybe (a,Int,Env)}

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> Just (x,fst s,snd s))
    m >>= f  = StateErrorTick (\s -> let run = runStateErrorTick m (0,snd s)
                                     in case run of
                                          Nothing      -> Nothing
                                          Just (a,i,e) -> runStateErrorTick (f a) ((fst s)+i,e))


-- Clase para representar monadas con estados variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateErrorTick where
    lookfor v = StateErrorTick (\s -> Just (lookfor' v (snd s), fst s, snd s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = StateErrorTick (\s -> Just ((), (fst s), update' v i (snd s)))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)


-- Clase para representar monadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\s -> Nothing)


-- Clase para representar monadas que lanzan errores
class Monad m => MonadTick m where
    -- Cuenta las operaciones
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> Just ((), 1, snd s) ) 

-- Estado nulo
initState :: Env
initState = []

-- Evalua un programa en el estado nulo
eval :: Comm -> (Env,Int)
eval p = case (runStateErrorTick (evalComm p) (0,initState)) of
           Nothing -> (initState,0)
           Just (a,i,e) -> (e,i)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m,MonadError m,MonadTick m) => Comm -> m ()
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
evalIntExp :: (MonadState m,MonadError m,MonadTick m) => IntExp -> m Int
evalIntExp (Const i)   = return i
evalIntExp (Var var)   = do v <- lookfor var
                            return v
evalIntExp (UMinus i)  = do x <- evalIntExp i
                            tick
                            return (-x)
evalIntExp (Plus i j)  = do x <- evalIntExp i
                            y <- evalIntExp j
                            tick
                            return (x+y)
evalIntExp (Minus i j) = do x <- evalIntExp i
                            y <- evalIntExp j
                            tick
                            return (x-y)
evalIntExp (Times i j) = do x <- evalIntExp i
                            y <- evalIntExp j
                            tick
                            return (x*y)
evalIntExp (Div i j)   = do x <- evalIntExp i
                            y <- evalIntExp j
                            if y == 0 then throw
                                      else do tick
                                              return (div x y)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m,MonadError m,MonadTick m) => BoolExp -> m Bool
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
