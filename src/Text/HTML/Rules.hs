{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : Text.HTML.Rules
Description : Transform an HTML structure by sets of rules.
Copyright   : (c) Kyle Carter, 2014
License     : BSD3
Maintainer  : kylcarte@gmail.com
Stability   : experimental
-}

module Text.HTML.Rules
  ( module Text.HTML.Rules
  , module M
  ) where

import Text.HTML.Rules.Types      as M
import Text.HTML.Rules.Apply      as M
import Text.HTML.Rules.Query      as M
import Text.HTML.Rules.Transform  as M
import Text.HTML.Rules.Attributes as M

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

-- * Example

{-
-- {{{
-- | an example set of rules.
testRules :: Monad m => [FragR m]
testRules =
  [ ( tag "row"                             -- match tags with the 'row' name.
    , one $ do                              -- return a single fragment of HTML.
      _Tag   *-> "div"                      -- replace the tag name with 'div'.
      _Attrs $-> addClass "row"             -- add 'row' to the list of classes,
                                            -- keeping all existing attributes.
    )
  , ( tag "col"                             -- match 'col' tags.
    , one $ do                              -- ..
      _Tag   *-> "div"                      -- ..
      _Attrs $-> do                         -- ..
        w  <- required $ _Attr "width"      -- pull out the 'width' attribute,
                                            -- failing if it isn't present.
        ms <- optional $ _Attr "size"       -- pull out the 'size' attribute,
                                            -- if it is present.
        addClass $ concat                   -- add a Bootstrap-friendly class,
                                            -- e.g. 'col-md-3'.
          [ "col-"
          , fromMaybe "xs" ms
          , "-"
          , w
          ]
    )
  , ( tags ["ol","ul"]                      -- match tags with either the 'ol' or 'ul' name.
    , one $ do                              -- ..
      _Attrs $-> addClass "list-unstyled"   -- add 'list-unstyled' to its list of classes.
    )
  , ( tag "expand"                          -- match 'expand' tags.
    , onEach (_Attrs $-> addClass "bar")    -- add 'bar' to the list of classes of each of the following:
      $ return                              --
      [ Leaf "expanded" [("class","foo")]   -- a leaf tag, with no descendents.
      , Branch "expanded-also" []           -- a branch tag, with one descendent.
        [ Leaf "yet-another" []             -- ..
        ]                                   -- ..
      , Text "some text"                    -- a text node.
      ]
    )
  ]
-- }}}
-}

-- | an example set of rules.
testRules :: Monad m => [FragR m]
testRules =
  [ ( tag "row"
    , one $ do
      _Tag   *-> "div"
      _Attrs $-> addClass "row"
    )
  --------
  , ( tag "col"
    , one $ do
      _Tag   *-> "div"
      _Attrs $-> do
        w  <- required $ _Attr "width"
        ms <- optional $ _Attr "size"
        addClass $ concat
          [ "col-"
          , fromMaybe "xs" ms
          , "-"
          , w
          ]
    )
  --------
  , ( tags ["ol","ul"]
    , one $ do
      _Attrs $-> addClass "list-unstyled"
    )
  --------
  , ( tag "expand"
    , onEach (_Attrs $-> addClass "bar")
      $ return
      [ Leaf "expanded" [("class","foo")]
      , Branch "expanded-also" []
        [ Leaf "yet-another" []
        ]
      , Text "some text"
      ]
    )
  ]

runTest :: IO ()
runTest = do
  putStrLn "Examples:\n"
  forM_ examples $ \s -> do
    s' <- applyHTMLRules testRules s
    putStrLn $ unlines
      [ s
      , "===>"
      , s'
      ]

examples :: [String]
examples =
  [ "<row>some text</row>"
  , "just some text"
  , "<row><col width=\"3\">nested</col>a bit more text</row>"
  , "<expand class=\"these get lost in expansion\">"
  ]


