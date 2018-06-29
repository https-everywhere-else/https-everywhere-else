module Data.Text.RadixTrie (
  RadixTrie,
  empty,
  lookup,
  lookupDefault,
  elem,
  insert,
  enumerate
) where

import Prelude hiding (lookup, elem)
import Data.ByteString.RadixTrie (RadixTrie, empty)
import qualified Data.ByteString.RadixTrie as RT
import qualified Data.Text as Strict (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

lookup :: Strict.Text -> (RadixTrie a -> Maybe a)
lookup t r = RT.lookup (encodeUtf8 t) r

lookupDefault :: a -> Strict.Text -> (RadixTrie a -> a)
lookupDefault x t r = RT.lookupDefault x (encodeUtf8 t) r

elem :: Strict.Text -> (RadixTrie a -> Bool)
elem t r = RT.elem (encodeUtf8 t) r

insert :: Strict.Text -> a -> (RadixTrie a -> RadixTrie a)
insert t x r = RT.insert (encodeUtf8 t) x r

enumerate :: RadixTrie a -> [(Strict.Text, a)]
enumerate t = map (\(x, y) -> (decodeUtf8 x, y)) (RT.enumerate t)
