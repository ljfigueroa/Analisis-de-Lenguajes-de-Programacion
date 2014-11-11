import Prelude hiding (mapM)
import Data.Char


--Ej 9

--a)
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = f x >>= \k -> 
  do ls <- mapM f xs
     return (k : ls)
     
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = f x >>= \k -> mapM' f xs >>= \ls -> return (k:ls)

--b)
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f e [] = return e
foldM f e (x:xs) = do e1 <- f e x
                      ef <- foldM f e1 xs
                      return ef
                      
                      
--Ej 13

holaMundo :: IO ()
holaMundo =  aux (map putChar "Hola mundo! \n")

aux :: [IO ()] -> IO ()
aux [] = return ()
aux (x:xs) = do x
                aux xs
           
hola :: IO ()
hola = printf "Hola!! \n"

printf :: String -> IO ()
printf [] = return ()
printf (x:xs) = do putChar x
                   printf xs
                   
            
--Ej 14

secretNum :: Int
secretNum = 17

isSecretNum :: Int -> Int
isSecretNum ns | ns == secretNum = 0
               | ns <  secretNum = -1
               | otherwise       = 1

                                   
gameMain :: String -> IO ()
gameMain [] = print "Waat!, ingresa algún número para comparar ¬¬ \n"
gameMain xs = 
  case isSecretNum (read xs) of 
       0    -> printf "Ganaste!! :D \n"
       (-1) -> do printf "Oops, proba un número mas grande \n"
                  s <-getLine
                  gameMain (read s)
       1    -> do printf "Oops, proba un número menor \n"
                  s <- getLine
                  gameMain (read s)


gameMain2 :: Int -> IO ()
gameMain2 x = 
  case isSecretNum x of 
       0    -> printf "Ganaste!! :D \n"
       (-1) -> do printf "Oops, proba un número mayor \n"
                  printf ":> "
                  s <-getLine
                  gameMain2 ((read s)::Int)
       1    -> do printf "Oops, proba un número menor \n"
                  printf ":> "
                  s <- getLine
                  gameMain2 ((read s)::Int)




--Ej 15


type Tablero = [Int]
type Posicion = (Int,Int)

tableroInicial :: Tablero
tableroInicial = [5,4,3,2,1]


nim :: IO ()
nim  = do elim <- input
          evalnim tableroInicial elim
evalnim :: Tablero -> Posicion -> IO ()
evalnim t (f,s) =

