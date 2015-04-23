{-# LANGUAGE TypeSynonymInstances #-}

module Parser(topLevel) where

type Parser a = String -> Maybe (a, String)

instance Monad Parser where
  return :: a -> Parser a
  return x = (\inp-> Just (x, inp))

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = (\inp-> case p inp of
                      Just (x, rest) -> f x rest
                      Nothing -> Nothing)

topLevel :: Int
topLevel = 1