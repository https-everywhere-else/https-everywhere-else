{-# LANGUAGE PackageImports #-}

module Main (main) where

import "foldl" Control.Foldl (Fold(..))
import qualified "foldl" Control.Foldl as Foldl
import "base" Control.Monad (forever)
import "base" Data.Bool (bool)
import "https-everywhere-rules" Data.HTTPSEverywhere.Rules (rewriteURL, RuleSet, getRulesets)
import "base" Data.Maybe (isJust)
import "base" Data.Monoid (Sum(..))
import "network-uri" Network.URI (URI, parseURI)
import "pipes" Pipes (Pipe, Producer, lift, yield, await, (>->))
import "pipes" Pipes.Prelude (stdinLn)
import qualified "pipes" Pipes.Prelude as Pipes (fold)

parse :: Monad m => Pipe String URI m ()
parse = forever $ parseURI <$> await >>= maybe (return ()) yield

check :: [RuleSet] -> Pipe URI Bool IO ()
check rs = forever $ await >>= yield . isJust . rewriteURL rs

fold :: Monad m => Fold a b -> Producer a m () -> m b
fold (Fold step begin done) = Pipes.fold step begin done

proportion :: Fold Bool (Int, Int)
proportion = (,) <$> Foldl.foldMap (Sum . bool 0 1) getSum <*> Foldl.length

main :: IO ()
main = do
  rulesets <- getRulesets
  fold proportion (stdinLn >-> parse >-> check rulesets) >>= print
