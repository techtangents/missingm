module Control.Monad.MissingM where

findM ::
  Monad m =>
  (a -> m Bool)
  -> [a]
  -> m (Maybe a)
findM _ [] =
  return Nothing
findM f (x:xs) =
  do
    b <- f x
    if b
      then (return . Just) x
      else findM f xs
