{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveLift          #-}

module Data.HTTPSEverywhere.Rules.Internal.Trie (
  RuleTrie,
  RuleSet,
  empty,
  insert,
  lookup,
  enumerate
) where

import Prelude hiding (lookup, null)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, null, split, snoc, append)
import Language.Haskell.TH.Syntax (Lift)

import Data.Text.RadixTrie (RadixTrie)
import qualified Data.Text.RadixTrie as RadixTrie
import qualified
  Data.HTTPSEverywhere.Rules.Internal.Types as Types

data Trie a = Trie
  { rule :: Maybe a -- set if terminal
  , wildcard :: Maybe a
  , children :: RadixTrie (Trie a)
  } deriving (Show, Functor, Lift)

data RuleSet a = RuleSet { wild :: Trie a, tame :: Trie a }
  deriving (Show, Functor, Lift)

type RuleTrie = RuleSet Types.RuleSet

snoc' :: Text -> Char -> Text
snoc' txt c = if null txt then txt else snoc txt c

enumerate' :: Trie a -> [(Text, a)]
enumerate' (Trie r w c) =
  maybe [] (\x -> [("",  x)]) r ++
  maybe [] (\x -> [("*", x)]) w ++
  concatMap (\(x, y) -> map (\(a, b) -> (append (snoc' a '.') x, b)) (enumerate' y)) (RadixTrie.enumerate c)

enumerate :: RuleSet a -> [(Text, a)]
enumerate (RuleSet wild tame) =
  (map (\(x, y) -> (append x ".*", y)) (enumerate' wild)) ++
  enumerate' tame

emptyTrie :: Trie a
emptyTrie = Trie Nothing Nothing RadixTrie.empty

empty :: RuleSet a
empty = RuleSet emptyTrie emptyTrie

isDot :: Char -> Bool
isDot = (== '.')

insert' :: ([Text], a) -> (Trie a -> Trie a)
insert' ([], a) trie = trie { rule = return a }
insert' (t : ts, a) trie
  | t == "*" = trie { wildcard = return a }
  | otherwise = trie { children =
                         RadixTrie.insert t
                         (insert' (ts, a)
                          (RadixTrie.lookupDefault emptyTrie
                           t (children trie)))
                         (children trie) }

insert :: (Text, a) -> (RuleSet a -> RuleSet a)
insert (s, a) rs =
  let parts@(_:_) = reverse (split isDot s) in
  if head parts == "*"
    then rs { wild = insert' (tail parts, a) (wild rs) }
    else rs { tame = insert' (parts, a) (tame rs) }

lookup' :: Trie a -> ([Text] -> [a])
lookup' (Trie rule _ _) [] = maybeToList rule
lookup' (Trie _ wild children) (sub : subsub) =
  fromMaybe [] (fmap (flip lookup' subsub)
                (sub `RadixTrie.lookup` children)) ++
  maybeToList wild

lookup :: RuleSet a -> (Text -> [a])
lookup RuleSet{..} host =
  let parts@(_:_) = reverse (split isDot host) in
  lookup' tame parts ++ lookup' wild (tail parts)
