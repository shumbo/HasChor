{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Data.Proxy
import Data.Time
import GHC.TypeLits (KnownSymbol)
import System.Environment

divide :: [a] -> ([a], [a])
divide xs = splitAt lhx xs
  where
    lhx = length xs `div` 2

primary :: Proxy "primary"
primary = Proxy

worker1 :: Proxy "worker1"
worker1 = Proxy

worker2 :: Proxy "worker2"
worker2 = Proxy

sort ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  KnownSymbol c =>
  Proxy c ->
  ([Int] @ a) ->
  Choreo IO ([Int] @ a)
sort a b c lst = do
  condition <- a `locallyDo` \unwrap -> do return $ length (unwrap lst) > 1
  cond a condition \case
    True -> do
      pivot <- a `locallyDo` \unwrap -> do return $ length (unwrap lst) `div` 2
      divided <- a `locallyDo` \unwrap -> do return $ divide (unwrap lst)
      l <- a `locallyDo` \unwrap -> do return $ fst (unwrap divided)
      r <- a `locallyDo` \unwrap -> do return $ snd (unwrap divided)
      l' <- (a, l) ~> b
      r' <- (a, r) ~> c
      ls' <- sort b c a l'
      rs' <- sort c a b r'
      merge a b c ls' rs'
    False -> do
      return lst

merge ::
  KnownSymbol a =>
  Proxy a ->
  KnownSymbol b =>
  Proxy b ->
  KnownSymbol c =>
  Proxy c ->
  [Int] @ b ->
  [Int] @ c ->
  Choreo IO ([Int] @ a)
merge a b c lhs rhs = do
  lhsHasElements <- b `locallyDo` \unwrap -> do return $ not (null (unwrap lhs))
  cond b lhsHasElements \case
    True -> do
      rhsHasElements <- c `locallyDo` \unwrap -> do return $ not (null (unwrap rhs))
      cond c rhsHasElements \case
        True -> do
          rhsHeadAtC <- c `locallyDo` \unwrap -> do return $ head (unwrap rhs)
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b
          takeLhs <- b `locallyDo` \unwrap -> do return $ head (unwrap lhs) <= unwrap rhsHeadAtB
          cond b takeLhs \case
            True -> do
              -- take (head lhs) and merge the rest
              lhs' <- b `locallyDo` \unwrap -> do return $ tail (unwrap lhs)
              merged <- merge a b c lhs' rhs
              lhsHeadAtB <- b `locallyDo` \unwrap -> do return $ head (unwrap lhs)
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a
              a `locallyDo` \unwrap -> do return $ unwrap lhsHeadAtA : unwrap merged
            False -> do
              -- take (head rhs) and merge the rest
              rhs' <- c `locallyDo` \unwrap -> do return $ tail (unwrap rhs)
              merged <- merge a b c lhs rhs'
              rhsHeadAtC <- c `locallyDo` \unwrap -> do return $ head (unwrap rhs)
              rhsHeadAtA <- (c, rhsHeadAtC) ~> a
              a `locallyDo` \unwrap -> do return $ unwrap rhsHeadAtA : unwrap merged
        False -> do
          (b, lhs) ~> a
    False -> do
      (c, rhs) ~> a

mainChoreo :: Choreo IO ()
mainChoreo = do
  lst <- primary `locallyDo` \unwrap -> do return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  primary `locallyDo` \unwrap -> do
    print (unwrap sorted)
    return ()
  return ()

main :: IO ()
main = do
  runChoreo mainChoreo
