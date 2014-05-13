{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Util
Description : Transform an HTML structure by sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Util where

import Control.Applicative
import Control.Monad ((>=>))

{-
-- ** Traversal Functions

alterOf :: (Cons s s c c, Monad m) => Traversal s t (Maybe a,s) b -> (Maybe a -> Maybe c) -> Trans' m s
alterOf l f = do
  mv <- optional l
  case f mv of
    Just v -> with (<|) v
    _      -> get

insertOf :: (Cons s s r r, Monad m) => Traversal s t (Maybe a,s) b -> (Maybe a -> r) -> Trans' m s
insertOf l = alterOf l . (return .)

updateOf :: (Cons s s r r, Monad m) => Traversal s t (Maybe a,s) b -> (a -> r) -> Trans' m s
updateOf l = alterOf l . fmap

adjustOf :: (Cons s s r r, Monad m) => Traversal s t (Maybe a,s) b -> (a -> Maybe r) -> Trans' m s
adjustOf l = alterOf l . (=<<)
-}


-- | bind into a pure function.
(>>$) :: Monad m => m a -> (a -> b) -> m b
m >>$ f = m >>= return . f
infixl 1 >>$

-- | compose monadic sequent function with a pure function, left to right.
(>$>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
f >$> g = f >=> return . g
infixr 1 >$>

-- | compose monadic sequent function with a pure function, right to left.
(<$<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c
(<$<) = flip (>$>)
infixr 1 <$<

-- | construct a singleton list.
one_ :: a -> [a]
one_ = (:[])

-- | compse a list of sequent endo-functions into a single sequent.
concatEndo :: Monad m => [a -> m a] -> a -> m a
concatEndo = foldr (>=>) return

liftAA :: (Applicative f, Applicative g) => (a -> b) -> f (g a) -> f (g b)
liftAA = fmap . fmap

pure2 :: (Applicative f, Applicative g) => a -> f (g a)
pure2 = pure . pure

ap2   :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
ap2 f x = (<*>) <$> f <*> x

liftAA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftAA2 f x y = pure2 f `ap2` x `ap2` y

