{-# OPTIONS_GHC -Wno-orphans #-}
module Examples.MutualFunds.Sale where

import Control.SimpleStateMachine (MachineData (MachineData), StateMachine (..))
import Data.Kind (Type)
import Examples.MutualFunds.Common (
  Dollars,
  LogYield (..),
  MutualFund,
  OrderId,
  ShareUnits,
  InvestmentProcess (Sell),
 )

data SellState
  = -- | The sale has been initialized, but not yet listed.
    Pending
  | -- | The sale has been listed, and issued an order ID.
    Listed
  | -- | The sale has been closed, and the units have been sold.
    Closed
  | -- | The sale has failed.
    Failed
  deriving (Eq, Show)

instance (Monad m) => StateMachine m 'Sell SellState where
  data Props 'Sell = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data StateData 'Sell :: SellState -> Type where
    PendingData :: StateData 'Sell 'Pending
    ListedData :: OrderId -> StateData 'Sell 'Listed
    ClosedData :: StateData 'Sell 'Closed
    FailedData :: StateData 'Sell 'Failed

  data Transition 'Sell :: SellState -> SellState -> Type -> Type where
    List :: OrderId -> Transition 'Sell 'Pending 'Listed LogYield
    Close :: Dollars -> ShareUnits -> Transition 'Sell 'Listed 'Closed LogYield
    Fail :: Transition 'Sell 'Pending 'Failed LogYield

  data Init 'Sell :: SellState -> Type -> Type where
    InitPending :: Props 'Sell -> Init 'Sell 'Pending LogYield

  initialize :: forall (s0 :: SellState) yield. Init 'Sell s0 yield -> m (MachineData 'Sell s0, yield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transitionState ::
    forall (s1 :: SellState) (s2 :: SellState) yield.
    Transition 'Sell s1 s2 yield ->
    MachineData 'Sell s1 ->
    m (StateData 'Sell s2, yield)
  transitionState (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transitionState (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transitionState Fail _ = pure (FailedData, LogYield "Failed")