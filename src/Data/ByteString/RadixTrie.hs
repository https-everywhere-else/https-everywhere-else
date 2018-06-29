{-# LANGUAGE DeriveFunctor                 #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE RecordWildCards               #-}
{-# LANGUAGE TupleSections                 #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Data.ByteString.RadixTrie (
  RadixTrie,
  empty,
  lookup,
  lookupDefault,
  elem,
  enumerate,
  singleton,
  insert,
) where

import Prelude hiding
  (null, lookup, drop, length, head, tail, take, length, last, elem)
import Data.ByteString
  (ByteString, uncons, isPrefixOf, null, drop, length, head, tail,
   take, length, last, append, cons)
import Data.Vector (Vector, (!), generate, modify, imap)
import qualified Data.Vector as Vector (toList)
import qualified Data.Vector.Mutable as Mutable (modify)
import Data.Maybe (fromMaybe, isJust, maybeToList)

data Leaf a =
  Prefix ByteString (RadixTrie a) |
  Split (Vector {- 2 ^ 8 -} (Maybe (RadixTrie a))) |
  Vacuum deriving (Show, Functor)

data RadixTrie a =
  RadixTrie { label :: Maybe a, children :: Leaf a }
  deriving (Show, Functor)

empty :: RadixTrie a
empty = RadixTrie Nothing Vacuum

lookup :: ByteString -> (RadixTrie a -> Maybe a)
lookup str RadixTrie{..}
  | null str = label
  | otherwise = case children of
      Prefix prefix child ->
        if prefix `isPrefixOf` str
        then drop (length prefix) str `lookup` child
        else Nothing
      Split vector -> do
        (c, cs) <- uncons str
        trie <- vector ! fromIntegral c
        lookup cs trie
      Vacuum -> Nothing

lookupDefault :: a -> ByteString -> (RadixTrie a -> a)
lookupDefault def = fmap (fmap (fromMaybe def)) lookup

elem :: ByteString -> (RadixTrie a -> Bool)
elem = fmap (fmap isJust) lookup

enumerate :: RadixTrie a -> [(ByteString, a)]
enumerate (RadixTrie label (Prefix prefix trie)) =
  maybe [] (\x -> [("", x)]) label ++
  map (\(x, y) -> (prefix `append` x, y)) (enumerate trie)
enumerate (RadixTrie label (Split vector)) =
  maybe [] (\x -> [("", x)]) label ++
  concat (Vector.toList
          (imap (\i t -> maybe []
                  (map (\(x, y) ->
                          (cons (fromIntegral i) x, y)) . enumerate) t)
            vector))
enumerate (RadixTrie a Vacuum) = map ("",) (maybeToList a)

(?=) :: Eq a => a -> a -> Maybe ()
m ?= n = if m == n then return () else Nothing

singleton' :: ByteString -> (RadixTrie a -> RadixTrie a)
singleton' str end
  | null str = end
  | length str == 1 = empty { children =
      Split . generate (2 ^ (8 :: Int)) $ \i -> do
        _ <- fromIntegral i ?= head str
        return end }
  | otherwise = empty { children = Prefix str end }

singleton :: ByteString -> a -> RadixTrie a
singleton str value = singleton' str (empty { label = return value })

data PrefixResult =
  Equal |
  Disjoint |
  Extends ByteString |
  Prefixes ByteString

prefixResult :: ByteString -> ByteString -> PrefixResult
prefixResult source target =
  case (source `isPrefixOf` target, target `isPrefixOf` source) of
    (True, True) -> Equal
    (False, False) -> Disjoint
    (True, False) -> Extends $ drop (length source) target
    (False, True) -> Prefixes $ drop (length target) source

update :: Int -> (Maybe a -> a) -> (Vector (Maybe a) -> Vector (Maybe a))
update n f v = modify (\v' -> Mutable.modify v' (Just . f) n) v

insert :: ByteString -> a -> (RadixTrie a -> RadixTrie a)
insert (uncons -> Just (fromIntegral -> c, cs)) val
  r@(RadixTrie _ (Split split)) = r { children = Split $
    update c (maybe (singleton cs val) (insert cs val)) split }
insert _ val r@(RadixTrie _ (Split _)) = r { label = return val }
insert text val r@(RadixTrie label (Prefix prefix trie)) =
  case prefixResult prefix text of
    Equal -> r { children = Prefix text trie }
    Extends e -> r { children = Prefix prefix $ insert e val trie }
    Disjoint ->
      r { children = Split . generate (2 ^ (8 :: Int)) $ \i ->
            if i == fromIntegral (head text)
            then return $ singleton (tail text) val
            else if i == fromIntegral (head prefix)
            then return $ singleton' (tail prefix) trie
            else Nothing }
    Prefixes p -> case length text of
      0 -> r { label = return val }
      1 -> r { children = Split . generate (2 ^ (8 :: Int)) $ \i ->
                 if i == fromIntegral (head text) then
                   return $ singleton (tail text) val
                 else if i == fromIntegral (head prefix) then
                   return $ singleton' (tail prefix) trie
                 else Nothing }
      _ -> RadixTrie label . Prefix (take (length text) prefix) $
           case length prefix - length text of
             0 -> error "The impossible happened!"
             1 -> RadixTrie (Just val)
               (Split . generate (2 ^ (8 :: Int)) $ \i ->
                 fmap (const trie) (i ?= fromIntegral (last prefix)))
             _ -> RadixTrie (Just val) (Prefix p trie)
insert text val (RadixTrie value Vacuum) =
  (singleton' text (empty { label = return val })) { label = value }

example :: RadixTrie ByteString
example = foldr (.) id insertions empty where
  insertions =
    map (\x -> insert x x) ["test", "slow", "slower", "water"]
