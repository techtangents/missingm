module Control.Monad.MissingM where

import Data.Maybe (isJust)

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

findMapM ::
  Monad m =>
  (a -> m (Maybe b))
  -> [a]
  -> m (Maybe b)
findMapM _ [] = return Nothing
findMapM f (x:xs) =
  do
    mb <- f x
    if (isJust mb)
      then return mb
      else findMapM f xs
