{-# OPTIONS_GHC -Wno-orphans #-}

module Examples.MutualFunds.Buy (
  BuyState (..),
  Props (..),
  StateData (..),
  Init (..),
  Transition (..),
) where

import Control.SimpleStateMachine (MachineData (MachineData), StateMachine (..))
import Data.Kind (Type)
import Examples.MutualFunds.Common (
  Dollars,
  LogYield (..),
  MutualFund,
  OrderId,
  ShareUnits,
  InvestmentProcess (Buy),
 )

data BuyState
  = Pending
  | Listed
  | Closed
  | Failed
  deriving (Eq, Show)

instance (Monad m) => StateMachine m 'Buy BuyState where
  data Props 'Buy = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data StateData 'Buy :: BuyState -> Type where
    PendingData :: StateData 'Buy 'Pending
    ListedData :: OrderId -> StateData 'Buy 'Listed
    ClosedData :: StateData 'Buy 'Closed
    FailedData :: StateData 'Buy 'Failed

  data Transition 'Buy :: BuyState -> BuyState -> Type -> Type where
    List :: OrderId -> Transition 'Buy 'Pending 'Listed LogYield
    Close :: Dollars -> ShareUnits -> Transition 'Buy 'Listed 'Closed LogYield
    Fail :: Transition 'Buy 'Pending 'Failed LogYield

  data Init 'Buy :: BuyState -> Type -> Type where
    InitPending :: Props 'Buy -> Init 'Buy 'Pending LogYield

  initialize ::
    forall (s0 :: BuyState) yield.
    Init 'Buy s0 yield ->
    m (MachineData 'Buy s0, yield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transitionState ::
    forall (s1 :: BuyState) (s2 :: BuyState) yield.
    Transition 'Buy s1 s2 yield ->
    MachineData 'Buy s1 ->
    m (StateData 'Buy s2, yield)
  transitionState (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transitionState (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transitionState Fail _ = pure (FailedData, LogYield "Failed")
