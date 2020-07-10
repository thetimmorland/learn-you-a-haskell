-- a functor is a "computational context"

-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

-- data CMaybe a = CNothing | CJust Int a deriving (Show)

-- This isn't a functor since it doesn't obey (fmap id x) == x
-- instance Functor CMaybe where
--     fmap f CNothing = CNothing
--     fmap f (CJust counter x) = CJust (counter + 1) (f x)

import qualified Control.Applicative           as A
import qualified Data.Foldable                 as F

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (A.liftA2 (:)) (pure [])

data Tree a = Empty
            | Node a (Tree a) (Tree a)
                deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r
