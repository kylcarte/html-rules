{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Apply
Description : Functions for applying sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Apply where

import Text.HTML.Rules.Transform
import Text.HTML.Rules.Types
import Text.HTML.Rules.Util

import Control.Lens
import Control.Applicative hiding (optional)
import Control.Monad.State

import Text.HTML.TagSoup hiding (Tag)
import Text.HTML.TagSoup.Tree


-- | a parsing/rendering Isomorphism.
_HTML :: Iso' String HTML
_HTML = iso (tagTree . parseTags) (renderTags . flattenTree)

-- | a traversal over HTML under parsing
onHTML :: Functor m => (HTML -> m HTML) -> String -> m String
onHTML = traverseOf _HTML

-- | apply all rules, left to right, bottom up.
applyHTMLRules :: (Functor m, Monad m) => [FragR m] -> String -> m String
applyHTMLRules = traverseOf _HTML . htmlRules

-- | appy all rules, left to right, top down.
applyHTMLRules' :: (Applicative m, Monad m) => [FragR m] -> String -> m String
applyHTMLRules' = traverseOf _HTML . htmlRules'

-- | apply all rules, left to right, bottom up.
applyRules :: (Functor m, Monad m) => Iso s t a b -> [Rule m a b] -> s -> m t
applyRules l = traverseOf l . rules_XXX

-- | appy all rules, left to right, top down.
applyRules' :: (Applicative m, Monad m) => Iso s t a b -> [Rule m a b] -> s -> m t
applyRules' l = traverseOf l . rules_XXX

rules_XXX :: Monad m => [Rule m a b] -> a -> m b
rules_XXX = undefined

-- | form a bottom-up traversal from a list of 'Rule's
htmlRules :: Monad m => [FragR m] -> HTML -> m HTML
htmlRules (joinRules -> go) = (concat <$<) $ mapM $ \t ->
  case t of
    TagBranch _ _ ds -> do
      ds' <- go ds
      go1 $ t & _Desc .~ ds'
    TagLeaf _ -> go1 t
  where
  go1 = go . one_

-- | form a top-down traversal from a list of 'Rule's
htmlRules' :: (Applicative m, Monad m) => [FragR m] -> HTML -> m HTML
htmlRules' (joinRules -> go) = (concat <$<) $ mapM $ \t ->
  case t of
    TagBranch {} -> mapM (traverseOf _Desc go) =<< go1 t
    TagLeaf   {} -> go1 t
  where
  go1 = go . one_

-- | join a list of 'Rule's into a traversal, left to right.
joinRules :: Monad m => [FragR m] -> HTML -> m HTML
joinRules = concatEndo . map rule

-- | make a traversal out of a single 'Rule'.
-- the 'Trans'formation is applied iff the query predicate holds.
rule :: Monad m => FragR m -> HTML -> m HTML
rule (q,m) = mapM go >$> concat
  where
  go x | q x  = evalStateT m x
       | True = return [x]

