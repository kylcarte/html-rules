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
  , module Text.HTML.Rules.Types
  , module Text.HTML.Rules.Apply
  , module Text.HTML.Rules.Query
  , module Text.HTML.Rules.Transform
  , module Text.HTML.Rules.Attributes
  ) where

import Text.HTML.Rules.Types
import Text.HTML.Rules.Apply
import Text.HTML.Rules.Query
import Text.HTML.Rules.Transform
import Text.HTML.Rules.Attributes

import Control.Applicative hiding (optional)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

-- * Example

-- | an example set of rules.
testRules :: (Applicative m, Monad m) => [FragmentRule m]
testRules =
  [ ( tag "row"                           -- <- match tags with the 'row' name.
    , one $ do                            -- <- return a single fragment of HTML.
      _Tag   *-> "div"                    -- <- replace the tag name with 'div'.
      _Attrs $-> addClass "row"           -- <- add 'row' to the list of classes,
                                          --    keeping all existing attributes.
    )
  ------------------------------------------------------------------------------
  , ( tag "col"                           -- <- match 'col' tags.
    , one $ do                            --
      _Tag   *-> "div"                    --
      _Attrs $-> do                       --
        w  <- required $ _Attr "width"    -- <- pull out the 'width' attribute,
                                          --    failing if it isn't present.
        ms <- optional $ _Attr "size"     -- <- pull out the 'size' attribute,
                                          --    if it is present.
        addClass $ concat                 -- <- add a Bootstrap-friendly
                                          --    class, e.g. 'col-md-3'.
          [ "col-"
          , fromMaybe "xs" ms
          , "-"
          , w
          ]
    )
  ------------------------------------------------------------------------------
  , ( tags ["ol","ul"]                    -- <- match tags with either the 'ol'
    , one $ do                            --    or 'ul' name.
      _Attrs $-> addClass "list-unstyled" -- <- add 'list-unstyled' to its list
    )                                     --    of classes.
  ------------------------------------------------------------------------------
  , ( tag "expand"                        -- <- match 'expand' tags.
    , onEach (_Attrs $-> addClass "bar")  -- <- add 'bar' to the list of classes
      $ return                            --    of each of the following:
      [ Leaf "expanded" [("class","foo")] --  * a leaf tag, with no descendents,
      , Branch "expanded-also" []         --  * a branch tag, with one descendent,
        [ Leaf "yet-another" []           --
        ]                                 --
      , Text "some text"                  --  * a text node.
      ]
    )
  ]

runTestExamples :: IO ()
runTestExamples = do
  putStrLn "Examples:\n"
  forM_ examples $ \s -> do
    s' <- applyHTMLRules testRules s
    putStrLn $ unlines [ s , "===>" , s' ]

examples :: [String]
examples =
  [ "<row>some text</row>"
  , "just some text"
  , "<row><col width=\"3\">nested</col>a bit more text</row>"
  , "<expand class=\"these get lost in expansion\">"
  ]

