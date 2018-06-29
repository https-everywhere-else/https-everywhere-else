{-# LANGUAGE TemplateHaskell #-}

module Data.HTTPSEverywhere.Rules.Internal.Trie.Supercompilation (
  trie
) where

import Control.Monad ((<=<))
import Pipes (Producer, for, each, yield, lift)
import Pipes.Prelude (fold)
import Language.Haskell.TH

import Data.HTTPSEverywhere.Rules.Internal.Parser (parseSerialisable)
import qualified Data.HTTPSEverywhere.Rules.Internal.Raw as Raw
  (getRule, getRules)
import qualified
  Data.HTTPSEverywhere.Rules.Internal.Types.Serialisable as Serialisable
import Data.HTTPSEverywhere.Rules.Internal.Trie (RuleSet, empty, insert)
import qualified
  Data.HTTPSEverywhere.Rules.Internal.Types as Types

getRulesets :: Producer Serialisable.RuleSet IO ()
getRulesets = lift Raw.getRules >>= (flip (for . each) (flip (for . each) yield <=< lift . (fmap parseSerialisable `fmap` Raw.getRule)))

getTrie :: IO (RuleSet Serialisable.RuleSet)
getTrie = fold
  (\tree rule ->
     foldr (.) id
     (map
      (\target -> insert (Serialisable.targetHost target, rule))
      (Serialisable.ruleSetTargets rule))
     tree)
  empty
  id
  getRulesets

-- getTrie :: IO RuleTrie
-- getTrie = fmap (fmap Types.fromSerialisable) getTrie'

trie :: Q Exp {- :: RuleTrie -}
trie = runIO getTrie >>= \t ->
  [| fmap Types.fromSerialisable t |]
