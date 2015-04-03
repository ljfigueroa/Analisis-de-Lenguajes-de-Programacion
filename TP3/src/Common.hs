module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
     |  Quote   Int
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base 
            | Unit
            | Tup Type Type
            | Fun Type Type
            | NatType
            deriving (Show, Eq)

{-  data NatType = NZero
               | NSuc NatType 
               deriving (Show, Eq)
-}

  -- Términos con nombres
  data LamTerm  =  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  Let String LamTerm LamTerm
                |  As  LamTerm Type
                |  LUnit
                |  LTup LamTerm LamTerm
                |  LFst LamTerm
                |  LSnd LamTerm
                |  LZero
                |  LSuc LamTerm
                |  LR LamTerm LamTerm LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | Tlet Term Term
             | Tas  Term Type
             | Tunit
             | Ttup Term Term
             | Tfst Term
             | Tsnd Term
             | Tzero
             | Tsuc Term
             | Trec Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VUnit 
             | VTup Value Value
             | VNat Term

  -- Contextos del tipado
  type Context = [Type]
