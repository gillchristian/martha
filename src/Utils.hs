module Utils (headOr, applyWhen, (&&^), (||^)) where

headOr :: a -> [a] -> a
headOr a [] = a
headOr _ (a : _) = a

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f a = f a
applyWhen False _ a = a

-- | The '&&' operator lifted to a monad. If the first
--   argument evaluates to 'False' the second argument will not
--   be evaluated.
--   Reference: <https://hackage.haskell.org/package/protolude-0.2.4/docs/src/Protolude.Bool.html#%26%26%5E protolude>.
infixr 3 &&^ -- same as (&&)

(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (return False)

-- | The '||' operator lifted to a monad. If the first
--   argument evaluates to 'True' the second argument will not
--   be evaluated.
--   Reference: <https://hackage.haskell.org/package/protolude-0.2.4/docs/src/Protolude.Bool.html#%7C%7C%5E protolude>.
infixr 2 ||^ -- same as (||)

(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
