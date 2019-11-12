module Utils (headOr, applyWhen) where

headOr :: a -> [a] -> a
headOr a [] = a
headOr _ (a : _) = a

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f a = f a
applyWhen False _ a = a
