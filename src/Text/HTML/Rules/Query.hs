{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Query
Description : Transform an HTML structure by sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Query where

import Text.HTML.Rules.Types
import Text.HTML.Rules.Util

import Control.Applicative hiding (optional)
import Data.List
import Data.Maybe

-- * Attribute-Specific Queries

-- | match any of the given 'Name's.
tags :: Applicative m => [Name] -> FragmentQuery m
tags = or_ . map tag
{-# INLINE tags #-}

-- | match a 'Name'.
tag :: Applicative m => Name -> FragmentQuery m
tag t = pure . \case
  Branch tg _ _ -> t == tg
  Leaf   tg _   -> t == tg
  _             -> False

-- | match a 'Text' node.
isText :: Applicative m => FragmentQuery m
isText = pure . \case
  Text _ -> True
  _      -> False

-- | match all nodes with a particular Attribute.
hasAttr :: Applicative m => AttrKey -> FragmentQuery m
hasAttr a = pure . \case
  Branch _ (yes -> r) _ -> isJust r
  Leaf   _ (yes -> r)   -> isJust r
  _                     -> False
  where
  yes = find ((== a) . fst)

-- * General Combinators

true, false :: Applicative m => Query m a
-- | always matches.
true  = pure2 True
-- | always fails.
false = pure2 False

-- ** Lifted Operations

-- | logical disjunction of 'Query's.
(.||.) :: Applicative m => Query m a -> Query m a -> Query m a
(.||.) = liftAA2 (||)
infixr 2 .||.

-- | logical disjunction of a list of 'Query's.
or_ :: Applicative m => [Query m a] -> Query m a
or_ = foldr (.||.) false
{-# INLINE or_ #-}

-- | logical conjunction of 'Query's.
(.&&.) :: Applicative m => Query m a -> Query m a -> Query m a
(.&&.) = liftAA2 (&&)
infixr 3 .&&.

-- | logical conjunction of a list of 'Query's.
and_ :: Applicative m => [Query m a] -> Query m a
and_ = foldr (.&&.) true
{-# INLINE and_ #-}

-- | negate the result of a 'Query'.
not_ :: Applicative m => Query m a -> Query m a
not_ = liftAA not

