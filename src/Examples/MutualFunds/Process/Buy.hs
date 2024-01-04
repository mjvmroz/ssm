{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.MutualFunds.Process.Buy (
  State (..),
  Props (..),
  StateData (..),
  Init (..),
  Transition (..),
) where

import Control.SimpleStateMachine (MachineData (MachineData), StateMachine (..))
import Data.Kind (Type)
import Examples.MutualFunds.Common (
  Dollars,
  InvestmentProcess (Buy),
  LogYield (..),
  MutualFund,
  OrderId,
  ShareUnits,
 )

data State
  = Pending
  | Listed
  | Closed
  | Failed
  deriving (Eq, Show)

instance (Monad m) => StateMachine m 'Buy State where
  data Props 'Buy = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data StateData 'Buy :: State -> Type where
    PendingData :: StateData 'Buy 'Pending
    ListedData :: OrderId -> StateData 'Buy 'Listed
    ClosedData :: StateData 'Buy 'Closed
    FailedData :: StateData 'Buy 'Failed

  data Transition 'Buy :: State -> State -> Type -> Type where
    List :: OrderId -> Transition 'Buy 'Pending 'Listed LogYield
    Close :: Dollars -> ShareUnits -> Transition 'Buy 'Listed 'Closed LogYield
    Fail :: Transition 'Buy 'Pending 'Failed LogYield

  data Init 'Buy :: State -> Type -> Type where
    InitPending :: Props 'Buy -> Init 'Buy 'Pending LogYield

  initialize ::
    forall (s0 :: State) yield.
    Init 'Buy s0 yield ->
    m (MachineData 'Buy s0, yield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transitionState ::
    forall (s1 :: State) (s2 :: State) yield.
    Transition 'Buy s1 s2 yield ->
    MachineData 'Buy s1 ->
    m (StateData 'Buy s2, yield)
  transitionState (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transitionState (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transitionState Fail _ = pure (FailedData, LogYield "Failed")

deriving instance Show (Props 'Buy)

instance forall (s :: State). Show (StateData 'Buy s) where
  show PendingData = "PendingData"
  show (ListedData orderId) = "ListedData " <> show orderId
  show ClosedData = "ClosedData"
  show FailedData = "FailedData"