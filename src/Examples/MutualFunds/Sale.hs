module Examples.MutualFunds.Sale where

import Control.SimpleStateMachine (MachineData (MachineData), StateMachine (..))
import Data.Kind (Type)
import Examples.MutualFunds.Common (
  Dollars,
  LogYield (..),
  MutualFund,
  OrderId,
  ShareUnits,
 )

data MutualFundSale = MutualFundSale deriving (Eq, Show)

data MutualFundSaleState
  = -- | The sale has been initialized, but not yet listed.
    Pending
  | -- | The sale has been listed, and issued an order ID.
    Listed
  | -- | The sale has been closed, and the units have been sold.
    Closed
  | -- | The sale has failed.
    Failed
  deriving (Eq, Show)

instance (Monad m) => StateMachine m MutualFundSale MutualFundSaleState where
  data Props MutualFundSale = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data StateData MutualFundSale :: MutualFundSaleState -> Type where
    PendingData :: StateData MutualFundSale 'Pending
    ListedData :: OrderId -> StateData MutualFundSale 'Listed
    ClosedData :: StateData MutualFundSale 'Closed
    FailedData :: StateData MutualFundSale 'Failed

  data Transition MutualFundSale :: MutualFundSaleState -> MutualFundSaleState -> Type -> Type where
    List :: OrderId -> Transition MutualFundSale 'Pending 'Listed LogYield
    Close :: Dollars -> ShareUnits -> Transition MutualFundSale 'Listed 'Closed LogYield
    Fail :: Transition MutualFundSale 'Pending 'Failed LogYield

  data Init MutualFundSale :: MutualFundSaleState -> Type -> Type where
    InitPending :: Props MutualFundSale -> Init MutualFundSale 'Pending LogYield

  initialize :: forall (s0 :: MutualFundSaleState) yield. Init MutualFundSale s0 yield -> m (MachineData MutualFundSale s0, yield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transitionState ::
    forall (s1 :: MutualFundSaleState) (s2 :: MutualFundSaleState) yield.
    Transition MutualFundSale s1 s2 yield ->
    MachineData MutualFundSale s1 ->
    m (StateData MutualFundSale s2, yield)
  transitionState (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transitionState (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transitionState Fail _ = pure (FailedData, LogYield "Failed")
