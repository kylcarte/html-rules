{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules.Transform
Description : Transform an HTML structure by sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules.Transform where

import Text.HTML.Rules.Types
import Text.HTML.Rules.Util

import Control.Lens
import Control.Lens.Internal.Context
import Control.Arrow (first)
import Control.Monad.Trans.Class
import Control.Monad.State
import Data.List
import Data.Maybe


-- * Transformations
-- {{{

-- ** Prisms and Traversals
-- {{{

-- | A pattern match onto an opening @Tag@.
_Open :: Prism' Fragment (Name,Attrs,Maybe Desc)
_Open = prism'
  (\(tg,as,mds) -> maybe (Leaf tg as) (Branch tg as) mds)
  (\case
    Branch tg as ds -> Just (tg,as,Just ds)
    Leaf   tg as    -> Just (tg,as,Nothing)
    _               -> Nothing)

-- | A pattern match onto a text @Tag@.
_Text :: Prism' Fragment String
_Text = prism' Text $
  \case
    Text s -> Just s
    _      -> Nothing

-- | A traversal to a @Tag@'s name.
_Tag :: Traversal' Fragment Name
_Tag = _Open . _1

-- | A traversal to a @Tag@'s Attributes.
_Attrs :: Traversal' Fragment Attrs
_Attrs = _Open . _2

-- | A traversal to a @Tag@'s descendents.
_Desc :: Traversal' Fragment Desc
_Desc = _Open . _3 . _Just

-- | A refining traversal to a particular attribute
-- in a list of attributes.
--
-- A refining traversal is one that additionally gives the
-- unmatched values. Used with 'on', this gives a stateful
-- filtering, so that matched Attributes are removed by default
-- from the attribute list.
_Attr :: AttrKey -> Lens Attrs Attrs (Maybe AttrVal,Attrs) Attrs
_Attr k = lens to_ from_
  where
  from_  = (++)
  to_ as = case ys of
    [(_,v)] -> (Just v  , ns)
    _       -> (Nothing , as)
    where
    (ys,ns) = partition ((== k) . fst) as

-- | give the first attribute satisfying some predicate. All other matching attributes
-- are dropped from the list.
_AttrWhere :: ((AttrKey,AttrVal) -> Bool) -> Traversal Attrs Attrs (Maybe (AttrKey,AttrVal),Attrs) Attrs
_AttrWhere pr = _Partition pr . travPre (first listToMaybe)

-- | give all attributes satisfying some predicate. The attribute list is refined
-- to only the non-matching attributes.
_AttrsWhere :: ((AttrKey,AttrVal) -> Bool) -> Traversal Attrs Attrs (Attrs,Attrs) Attrs
_AttrsWhere = _Partition

-- | give the first item from a list that satisfies a predicate. All other matches are
-- kept in the list, along with the non-matching items.
_First :: (a -> Bool) -> Traversal [a] b (Maybe a,[a]) b
_First pr = travPre $ \as ->
  let (ys,ns) = partition pr as
  in case ys of
    y : ys' -> (Just y  , ys' ++ ns)
    _       -> (Nothing , as)

-- | partition a list by a predicate. The satisfying items are given, and the others
-- are kept in the list.
_Partition :: (a -> Bool) -> Traversal [a] b ([a],[a]) b
_Partition = travPre . partition

-- }}}

-- ** Traversal Construction
-- {{{

-- | build a traversal from an 'in' and an 'out' function. Analogous to "Data.Profunctor"'s @dimap@
trav :: (s -> a) -> (b -> t) -> Traversal s t a b
trav in_ out_ = (. in_) . (fmap out_ .)
{-# INLINE trav #-}

-- | build a traversal that only operates going in. Analogous to "Data.Profunctor"'s @lmap@.
travPre  :: (s -> a) -> Traversal s t a t
travPre = flip trav id
{-# INLINE travPre #-}

-- | build a traversal that only operates going out. Analogous to "Data.Profunctor"'s @rmap@.
travPost :: (b -> t) -> Traversal s t s b
travPost = trav id
{-# INLINE travPost #-}

-- | alter a 'Trans'formation to forget the refined state that it was
-- carrying around.
forget :: ATraversal s t (a,u) b -> Traversal s t a b
forget f = cloneTraversal $ \g s -> runBazaar (f sell s) $ g . fst

-- | pass the initial @s@ as the refined state.
remember :: ATraversal s t a b -> Traversal s t (a,s) b
remember = modifyBy id

-- | refine the state by a given function.
modifyBy :: (s -> u) -> ATraversal s t a b -> Traversal s t (a,u) b
modifyBy f l = cloneTraversal $ \g s -> runBazaar (l sell s) $ g . flip (,) (f s)

-- }}}

-- ** Combinators
-- {{{

-- | lift an operation on one thing to an operation resulting in a list containing
-- only the one thing.
one :: Monad m => Trans m a b -> Trans m a [b]
one = (>>$ one_)

-- | perform a sub-'Trans'formation over each item resulting from
-- a higher 'Trans'formation. Essentially composition, with a resulting
-- list.
onEach :: Monad m => Trans m b c -> Trans m a [b] -> Trans m a [c]
onEach bc = (>>= mapM (lift . evalStateT bc))

-- | embed a sub-'Trans'formation into a larger context using a traversal.
-- viewing the state @s@ with the traversal provides the initial state
-- of the sub-'Trans'formation, and the resulting value is used to alter the
-- higher state, using 'set'.
on :: Monad m => Traversal s s a b -> Trans m a b -> Trans' m s
on l m = do
  s <- get
  case s ^? coerced l of
    Just a -> do
      b <- lift $ evalStateT m a
      with (set l) b
    _ -> return s

-- | fold a value into the state using some given function.
with :: Monad m => (a -> s -> s) -> a -> Trans' m s
with f a = state $ \s -> let s' = f a s in (s',s')
{-# INLINE with #-}

-- | shorthand for 'on'.
($->) :: Monad m => Traversal s s a b -> Trans m a b -> Trans' m s
($->) = on
infixr 0 $->

-- | return a pure value to 'on'.
(*->) :: Monad m => Traversal s s a b -> b -> Trans' m s
l *-> b = l $-> return b
infixr 0 *->

-- | pull out an item from a traversal of the 'Trans'formation's state.
-- fails if the traversal has no target.
required :: Monad m => Traversal s t (Maybe a,s) b -> Trans m s a
required l = optional l >>= \case
  Just a -> return a
  _      -> fail "required: No content in traversal"

-- | pull out an item from a traversal of the 'Trans'formation's state.
-- returns 'Just' the item if the traversal has a target, or
-- 'Nothing' if not.
optional :: Monad m => Traversal s t (Maybe a,s) b -> Trans m s (Maybe a)
optional l = do
  s <- get
  case s ^? coerced l of
    Just (a,s') -> put s' >> return a
    _           -> return Nothing

-- }}}

-- }}}

-- * Combinators
-- {{{

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

-- }}}

