{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples.MutualFunds.Common (
  MutualFund (..),
  OrderId,
  Dollars,
  ShareUnits,
  LogYield (..),
) where

data MutualFund = MULSX | VUSXX deriving (Eq, Ord, Show)
type OrderId = Int
type Dollars = Int
type ShareUnits = Int

newtype LogYield = LogYield String deriving (Eq, Show, Semigroup)