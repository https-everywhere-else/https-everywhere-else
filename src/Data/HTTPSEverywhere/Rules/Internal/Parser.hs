{-# LANGUAGE OverloadedStrings           #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.HTTPSEverywhere.Rules.Internal.Parser (
  parseRuleSets,
  parseSerialisable
) where

import Prelude hiding (take, drop)
import Control.Applicative ((<$>))
import Control.Lens (toListOf, only, to, (^..), (^.), (&), (<&>), _Just)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as Lazy (Text)
import Text.Taggy.Lens (html, allNamed, attr, Element)

import Data.HTTPSEverywhere.Rules.Internal.Types
  (RuleSet(..), fromSerialisable)
import qualified
  Data.HTTPSEverywhere.Rules.Internal.Types.Serialisable as Serialisable

parseRule :: Element -> Maybe Serialisable.Rule
parseRule element = Serialisable.Rule
  <$> element ^. attr "from"
  <*> element ^. attr "to"

parseCookieRule :: Element -> Maybe Serialisable.CookieRule
parseCookieRule element = Serialisable.CookieRule
  <$> element ^. attr "host"
  <*> element ^. attr "name"

parseSerialisable' :: Element -> Maybe Serialisable.RuleSet
parseSerialisable' txt = txt ^. attr "name" <&> \ruleSetName -> do
   let
     ruleSetTargets = txt ^.. allNamed (only "target")
       . attr "host"
       . _Just
       . to Serialisable.Target
     ruleSetRules = txt ^.. allNamed (only "rule")
       . to parseRule
       & catMaybes
     ruleSetExclusions = txt ^.. allNamed (only "exclusion")
       . attr "pattern"
       . _Just
       . to Serialisable.Exclusion
     ruleSetCookieRules = txt ^.. allNamed (only "securecookie")
       . to parseCookieRule
       & catMaybes
   Serialisable.RuleSet
     ruleSetName
     ruleSetTargets
     ruleSetRules
     ruleSetExclusions
     ruleSetCookieRules

parseSerialisable :: Lazy.Text -> [Serialisable.RuleSet]
parseSerialisable = fmap catMaybes `fmap` toListOf $
  html . allNamed (only "ruleset") . to parseSerialisable'

parseRuleSets :: Lazy.Text -> [RuleSet]
parseRuleSets = fmap fromSerialisable . parseSerialisable

