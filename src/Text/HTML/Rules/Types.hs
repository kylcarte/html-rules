{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Types
Description : Ubiquitous types for html-rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Types where

import Control.Monad.State

import Text.HTML.TagSoup hiding (Tag)
import Text.HTML.TagSoup.Tree


-- | a single HTML Tree
type Fragment    = TagTree String
-- | an HTML Forest
type HTML    = [Fragment]
-- | Tag name
type Name     = String
-- | Attribute name
type AttrKey    = String
-- | Attribute value
type AttrVal = String
type Attrs   = [Attribute String]
-- | an HTML branch's descendents
type Desc    = HTML


-- | a 'Query' to decide when to apply a transformation,
--   paired with the 'Trans'formation.
--
--   NB: a Transformation makes
--   a single 'Fragment' into zero or more 'Fragment's.
type Rule  m a b = (Query a, Trans m a b)

-- | a Rule over a 'Fragment' of HTML.
type FragR m     = Rule m Fragment HTML

-- | a predicate on some type
type Query a = a -> Bool
-- | a predicate on an HTML Fragment
type FragQ   = Query Fragment

-- | a Transformation with a State of @a@
type Trans  m a b = StateT a m b
-- | a reflexive Transformation, analogous to @(a -> a)@
type Trans' m a   = Trans m a a

-- * Pattern Synonyms

-- | @Branch tg as ds = TagBranch tg as ds@
pattern Branch tg as ds = TagBranch tg as ds

-- | @Leaf tg as = TagLeaf (TagOpen tg as)@
pattern Leaf tg as = TagLeaf (TagOpen tg as)

-- | @Text t = TagLeaf (TagText t)@
pattern Text t = TagLeaf (TagText t)

