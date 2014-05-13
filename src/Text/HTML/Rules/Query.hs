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

import Control.Applicative hiding (optional)
import Data.List
import Data.Maybe

-- * Attribute-Specific Queries

-- | match any of the given 'Name's.
tags :: [Name] -> FragQ
tags = or_ . map tag

-- | match a 'Name'.
tag :: Name -> FragQ
tag t = \case
  Branch tg _ _ -> t == tg
  Leaf   tg _   -> t == tg
  _             -> False

-- | match a 'Text' node.
isText :: FragQ
isText = \case
  Text _ -> True
  _      -> False

-- | match all nodes with a particular Attribute.
hasAttr :: AttrKey -> FragQ
hasAttr a = \case
  Branch _ (yes -> r) _ -> isJust r
  Leaf   _ (yes -> r)   -> isJust r
  _                     -> False
  where
  yes = find ((== a) . fst)

-- * General Combinators

true, false :: Query a
-- | always matches.
true  = const True
-- | always fails.
false = const False

-- ** Lifted Operations

-- | logical disjunction of 'Query's.
(.||.) :: Query a -> Query a -> Query a
(.||.) = liftA2 (||)
infixr 2 .||.

-- | logical disjunction of a list of 'Query's.
or_ :: [Query a] -> Query a
or_ = foldr (.||.) false
{-# INLINE or_ #-}

-- | logical conjunction of 'Query's.
(.&&.) :: Query a -> Query a -> Query a
(.&&.) = liftA2 (&&)
infixr 3 .&&.

-- | logical conjunction of a list of 'Query's.
and_ :: [Query a] -> Query a
and_ = foldr (.&&.) true
{-# INLINE and_ #-}

-- | negate the result of a 'Query'.
not_ :: Query a -> Query a
not_ = liftA not

