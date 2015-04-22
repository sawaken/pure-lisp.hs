import Parser as P
import Object as O


mkexp :: Obj
mkexp = O.Cons (O.Number 1) (O.Cons (O.Number 2) (O.Number 3))

main = do
  putStrLn ("hello, world" ++ (show P.topLevel))
  putStrLn (show mkexp)
  (putStrLn . show . O.atom) (O.Number 1)
  (putStrLn . show . O.atom) (O.Cons O.Nil O.Nil)
  (putStrLn . show) (O.eq (O.Number 1) (O.Number 2))

