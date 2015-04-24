module Parser(topLevel) where

import Control.Monad

newtype Parser a = P (String -> Maybe (a, String))

instance Monad Parser where
  -- return :: a -> Parser a
  return x = P (\inp-> Just (x, inp))

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp-> case parse p inp of
                        Just (x, rest) -> parse (f x) rest
                        Nothing -> Nothing)

instance MonadPlus Parser where
  mzero = P (\inp-> Nothing)
  mplus p q = P (\inp-> case parse p inp of
                          Just (x, rest) -> Just (x, rest)
                          Nothing -> parse q inp)

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

topLevel :: Int
topLevel = 1