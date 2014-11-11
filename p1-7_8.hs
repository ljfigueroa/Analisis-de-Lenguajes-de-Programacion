import Parsing
import Data.Char
import Data.Either


-- Ej 7 

{- Nota:

A = Ax | y  --> A = yA'
                A'= e | xA'

donde 'e' es la cadena vacia

-}

expr :: Parser Int
expr = do t <- term
          do e <- expr'
             return (e t)
           -- <|> do return t 

expr' :: Parser (Int -> Int)
expr' = do char '+'
           t <- term
           return (\e -> e + t)
         <|> do char '-'
                t <- term
                return (\e -> e - t)
              <|> do return (\e -> e)
        
term :: Parser Int
term = do f <- factor
          do  t <- term'
              return (t f)
           <|> do return f
                  
term' :: Parser (Int -> Int)
term' = do char '*'
           f <- factor
           return (\e -> e * f) 
         <|> do char '/'
                f <- factor
                return (\e -> (div) e f)
          
factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
          <|> do char '('
                 e <- expr
                 char ')'
                 return e


-- Ej 8
{-
declaration  type specifier declarator ’;’
declarator   ’*’ declarator | direct declarator
direct declarator direct declarator ’[’ constant expression ’]’ | ’(’ direct declarator ’)’ | identifier
type specifier  ’int’ | ’char’ | ’float’
constant expression  number


-}

