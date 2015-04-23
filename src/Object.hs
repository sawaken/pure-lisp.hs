module Object
( Env, Params, Body, Obj(..)
, atom, eq, car, cdr, cons
) where

import qualified Data.Map as Map

data Env = Env { parent :: Maybe Env
               , reftbl :: Map.Map Obj Obj} deriving (Show, Eq, Ord)

type Params = [Obj]
type Body = (Obj, [Obj])

data Obj = Number Int
  | Bool Bool
  | String String
  | Nil
  | Lambda Env Params Body
  | Symbol String
  | Cons Obj Obj deriving (Show, Eq, Ord)


atom :: Obj -> Obj
atom (Cons _ _) = Nil
atom _ = T

eq :: Obj -> Obj -> Obj
eq x y = if x == y then Bool True else Nil

car :: Obj -> Obj
car (Cons x _) = x

cdr :: Obj -> Obj
cdr (Cons _ y) = y

cons :: Obj -> Obj -> Obj
cons x y = Cons x y

