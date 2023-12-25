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

data MutualFundPurchase = MutualFundPurchase deriving (Eq, Show)

data MutualFundPurchaseState = Pending | Listed | Closed | Failed deriving (Eq, Show)

instance (Monad m) => StateMachine m MutualFundPurchase MutualFundPurchaseState LogYield where
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

  data Transition MutualFundPurchase :: MutualFundPurchaseState -> MutualFundPurchaseState -> Type where
    List :: OrderId -> Transition MutualFundPurchase 'Pending 'Listed
    Close :: Dollars -> ShareUnits -> Transition MutualFundPurchase 'Listed 'Closed
    Fail :: Transition MutualFundPurchase 'Pending 'Failed

  data Init MutualFundPurchase :: MutualFundPurchaseState -> Type where
    InitPending :: Props MutualFundPurchase -> Init MutualFundPurchase 'Pending

  initialize ::
    forall (s0 :: MutualFundPurchaseState).
    Init MutualFundPurchase s0 ->
    m (MachineData MutualFundPurchase s0, LogYield)
  initialize (InitPending props) = pure (MachineData props PendingData, LogYield "Initialized with Pending State")

  transition ::
    forall (s1 :: MutualFundPurchaseState) (s2 :: MutualFundPurchaseState).
    Transition MutualFundPurchase s1 s2 ->
    MachineData MutualFundPurchase s1 ->
    m (StateData MutualFundPurchase s2, LogYield)
  transition (List orderId) _ = pure (ListedData orderId, LogYield "Listed")
  transition (Close dollars units) _ = pure (ClosedData, LogYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transition Fail _ = pure (FailedData, LogYield "Failed")
