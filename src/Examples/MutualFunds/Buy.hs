module Examples.MutualFunds.Buy (
  MutualFundPurchase,
  MutualFundPurchaseState (..),
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
 )

data MutualFundPurchase = MutualFundPurchase deriving (Eq, Show)

data MutualFundPurchaseState = Pending | Listed | Closed | Failed deriving (Eq, Show)

instance (Monad m) => StateMachine m MutualFundPurchase MutualFundPurchaseState where
  data Props MutualFundPurchase = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data StateData MutualFundPurchase :: MutualFundPurchaseState -> Type where
    PendingData :: StateData MutualFundPurchase 'Pending
    ListedData :: OrderId -> StateData MutualFundPurchase 'Listed
    ClosedData :: StateData MutualFundPurchase 'Closed
    FailedData :: StateData MutualFundPurchase 'Failed

  data Transition MutualFundPurchase :: MutualFundPurchaseState -> MutualFundPurchaseState -> Type -> Type where
    List :: OrderId -> Transition MutualFundPurchase 'Pending 'Listed LogYield
    Close :: Dollars -> ShareUnits -> Transition MutualFundPurchase 'Listed 'Closed LogYield
    Fail :: Transition MutualFundPurchase 'Pending 'Failed LogYield

  data Init MutualFundPurchase :: MutualFundPurchaseState -> Type -> Type where
    InitPending :: Props MutualFundPurchase -> Init MutualFundPurchase 'Pending LogYield

  initialize ::
    forall (s0 :: MutualFundPurchaseState) yield.
    Init MutualFundPurchase s0 yield ->
    m (MachineData MutualFundPurchase s0, yield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transitionState ::
    forall (s1 :: MutualFundPurchaseState) (s2 :: MutualFundPurchaseState) yield.
    Transition MutualFundPurchase s1 s2 yield ->
    MachineData MutualFundPurchase s1 ->
    m (StateData MutualFundPurchase s2, yield)
  transitionState (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transitionState (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transitionState Fail _ = pure (FailedData, LogYield "Failed")
