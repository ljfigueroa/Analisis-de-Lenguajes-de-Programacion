import Parsing
import Data.Char
import Data.Either


data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show


-- Ej1
expr :: Parser Int
expr = do t <- term
          (do char '+'
              e <- expr
              return (t + e)
            <|> (do char '-'
                    e <- expr
                    return (t-e)
                  <|> return t))

term :: Parser Int
term = do f <- factor
          (do char '*'
              t <- term
              return (f * t)
            <|> (do char '/'
                    t <- term
                    return (f `div` t)
                  <|> return f))

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
         <|> do char '('
                e <- expr
                char ')'
                return e
-- Ej2
parser2 :: Parser a -> Parser a
parser2 p = do char '('
               e <- p
               char ')'
               return e
             <|> p

-- Ej3

exprE :: Parser Expr
exprE = do t <- termE
           (do char '+'
               e <- exprE
               return (BinOp Add t e)
             <|> (do char '-'
                     e <- exprE
                     return (BinOp Min t e)
                   <|> return t))

termE :: Parser Expr
termE = do f <- factorE
           (do char '*'
               t <- termE
               return (BinOp Mul f t)
             <|> (do char '/'
                     t <- termE
                     return (BinOp Div f t)
                   <|> return f))

factorE :: Parser Expr
factorE = do d <- digit
             return (Num (digitToInt d))
           <|> do char '('
                  e <- exprE
                  char ')'
                  return e       

-- Ej4
list :: Parser ([Either Int Char])
list = do char '['
          l <- list3
          char ']'
          return l

element3 :: Parser (Either Int Char)
element3 = do d <- digit
              return (Left (digitToInt d))
            <|> do char '´'
                   c <- (sat isAlpha)
                   char '´'
                   return (Right c)

list3 :: Parser ([Either Int Char])
list3 = do e <- element3
           (do char ','
               l <- list3
               return ([e]++l)
             <|> return [e])

--- Opcional --------------------------
element :: Parser (Either Int Char)
element = do d <- digit
             return (Left (digitToInt d))
           <|> do c <- (sat isAlpha)
                  return (Right c)

list2 :: Parser ([Either Int Char])
list2 = do e <- element
           (do char ','
               l <- list2
               return ([e]++l)
             <|> return [e])
----------------------------------------

--- Usando many
list4 :: Parser ([Either Int Char])
list4 = do char '['
           e <- element3
           es <- many (do char ','
                          element3)
           char ']'
           return (e:es)

--- Usando sepBy
list5 :: Parser ([Either Int Char])
list5 = do char '['
           es <- sepBy element3 (char ',')
           char ']'
           return es


-- Ej 5

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]

tipe :: Parser Basetype
tipe = do string "Int"
          return DInt
        <|> do string "Char"
               return DChar
             <|> do string "Float"
                    return DFloat

hasktypeList :: Parser Hasktype
hasktypeList = sepBy tipe (string " -> ")
               

-- Ej 6
data HaskType = Dint | Dchar | Fun HaskType HaskType deriving Show

tipo :: Parser HaskType
tipo = do string "Int"
          return Dint
        <|> do string "Char"
               return Dchar

haskTypeList :: Parser HaskType
haskTypeList = do t1 <- tipo 
                  (do string " -> "
                      t2 <- haskTypeList
                      return (Fun t1 t2)
                    <|> return t1)

