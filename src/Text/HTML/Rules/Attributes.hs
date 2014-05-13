{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Attributes
Description : Transform an HTML structure by sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Attributes where

import Text.HTML.Rules.Types
import Text.HTML.Rules.Transform

-- | add a single attribute key-value pair to a tag's attributes.
addAttr :: Monad m => (AttrKey,AttrVal) -> Trans' m Attrs
addAttr = with (:)

-- | add multiple attributes at once.
addAttrs :: Monad m => Attrs -> Trans' m Attrs
addAttrs = with (++)

-- | add an additional class value if the 'class' attribute is already
-- present, or just the given value if it is absent.
addClass :: Monad m => AttrVal -> Trans' m Attrs
addClass c' = insertAttr "class" $ maybe c' (++ " " ++ c')

-- | operate on an attribute's presence or absence.
alterAttr :: Monad m => AttrKey -> (Maybe AttrVal -> Maybe AttrVal) -> Trans' m Attrs
alterAttr k = alterOf (_Attr k) . mkAttr k

-- | add an attribute that may or may not already be present.
insertAttr :: Monad m => AttrKey -> (Maybe AttrVal -> AttrVal) -> Trans' m Attrs
insertAttr k = insertOf (_Attr k) . mkAttr' k

-- | update the value of an attribute, if it is present.
-- does nothing if the attribute is absent.
updateAttr :: Monad m => AttrKey -> (AttrVal -> AttrVal) -> Trans' m Attrs
updateAttr k = updateOf (_Attr k) . mkAttr' k

-- | either modify or remove an attribute, if it is present.
-- does nothing if otherwise.
adjustAttr :: Monad m => AttrKey -> (AttrVal -> Maybe AttrVal) -> Trans' m Attrs
adjustAttr k = adjustOf (_Attr k) . mkAttr k

mkAttr :: Functor f => AttrKey -> (a -> f AttrVal) -> a -> f (AttrKey, AttrVal)
mkAttr k = (.) $ fmap ((,) k)

mkAttr' :: AttrKey -> (a -> AttrVal) -> a -> (AttrKey, AttrVal)
mkAttr' k = ((,) k .)

