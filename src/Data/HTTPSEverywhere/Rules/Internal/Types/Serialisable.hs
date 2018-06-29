{-# Language DeriveLift #-}

module Data.HTTPSEverywhere.Rules.Internal.Types.Serialisable
where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift)
import Instances.TH.Lift ()

data Rule = Rule { ruleFrom :: Text, ruleTo :: Text }
  deriving (Show, Lift)

data Target = Target { targetHost :: Text }
  deriving (Show, Lift)

data Exclusion = Exclusion { exclusionPattern :: Text }
  deriving (Show, Lift)

data CookieRule = CookieRule
  { cookieRuleHost :: Text
  , cookieRuleName :: Text
  } deriving (Show, Lift)

data RuleSet = RuleSet
  { ruleSetName :: Text
  , ruleSetTargets :: [Target]
  , ruleSetRules :: [Rule]
  , ruleSetExclusions :: [Exclusion]
  , ruleSetCookieRules :: [CookieRule]
  } deriving (Show, Lift)
