{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}

-- This is a version of the `bookseller-1-simple` choreography, adapted for the benchmarking setup.
module Choreos.Bookseller where

import Data.Proxy
import Data.Time

import Benchmark
import Choreography hiding (cond, cond_, cond', cond_')

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

-- `bookseller'` is a simplified version of `bookseller` that utilizes `~~>`
bookseller' :: Cond_' -> Choreo IO ()
bookseller' cond_' = do
  title <- (buyer, \_ -> pure "Types and Programming Languages") ~~> seller

  price <- (seller, \un -> return $ priceOf (un title)) ~~> buyer

  cond_' (buyer, \un -> return $ (un price) < budget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un title)) ~~> buyer

      buyer `locally` \un -> do
        -- putStrLn $ "The book will be delivered on " ++ show (un deliveryDate)
        return $ Just (un deliveryDate)

    False -> do
      buyer `locally` \_ -> do
        -- putStrLn "The book's price is out of the budget"
        return Nothing

  pure ()

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120
priceOf _                                 = error "Unknown book"

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
deliveryDateOf _                                 = error "Unknown book"
