{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import System.Environment(getArgs)
import System.Exit(exitFailure)

import Benchmark
import Choreos.Bookseller
import Choreos.CleanRoom
import Choreos.Kvs

main :: IO ()
main = getArgs >>= \case
  [backend, cond, "bookseller"] -> benchmarkChoreography backend cond ["buyer", "seller"] bookseller'
  [backend, cond, "kvs"] -> benchmarkChoreography backend cond ["client", "primary", "backup"] kvs'
  [backend, cond, "clean-room", read -> numClients, read -> numFailures] ->
    let locs = cleanRoomLocs numClients
    in benchmarkChoreography backend cond locs $ mkCleanRoom numFailures locs
  _ -> putStrLn "Invalid args" *> exitFailure
