module MFBuy (
  MutualFundPurchase,
  MutualFundPurchaseState (..),
  Props (..),
  State (..),
  Init (..),
  Transition (..),
) where

import Data.Kind (Type)
import Machine

data MutualFund = MULSX | VUSXX deriving (Eq, Ord, Show)
type OrderId = Int
type Dollars = Int
type ShareUnits = Int

data MutualFundPurchase
data MutualFundPurchaseState = PendingS | ListedS | ClosedS | FailedS deriving (Eq, Show)

newtype MFBuyYield = MFBuyYield String deriving (Eq, Show)

instance (Monad m) => StateMachine m MutualFundPurchase MutualFundPurchaseState MFBuyYield where
  data Props MutualFundPurchase = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data State MutualFundPurchase :: MutualFundPurchaseState -> Type where
    Pending :: State MutualFundPurchase 'PendingS
    Listed :: OrderId -> State MutualFundPurchase 'ListedS
    Closed :: State MutualFundPurchase 'ClosedS
    Failed :: State MutualFundPurchase 'FailedS

  data Transition MutualFundPurchase :: MutualFundPurchaseState -> MutualFundPurchaseState -> Type where
    List :: OrderId -> Transition MutualFundPurchase 'PendingS 'ListedS
    Close :: Dollars -> ShareUnits -> Transition MutualFundPurchase 'ListedS 'ClosedS
    Fail :: Transition MutualFundPurchase 'PendingS 'FailedS

  data Init MutualFundPurchase :: MutualFundPurchaseState -> Type where
    InitPending :: Props MutualFundPurchase -> Init MutualFundPurchase 'PendingS

  initialize ::
    forall (s0 :: MutualFundPurchaseState).
    Init MutualFundPurchase s0 ->
    m (MachineData MutualFundPurchase s0, MFBuyYield)
  initialize (InitPending props) = pure (MachineData props Pending, MFBuyYield "Initialized with Pending State")

  transition ::
    forall (s1 :: MutualFundPurchaseState) (s2 :: MutualFundPurchaseState).
    Transition MutualFundPurchase s1 s2 ->
    MachineData MutualFundPurchase s1 ->
    m (State MutualFundPurchase s2, MFBuyYield)
  transition (List orderId) _ = pure (Listed orderId, MFBuyYield "Listed")
  transition (Close dollars units) _ = pure (Closed, MFBuyYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transition Fail _ = pure (Failed, MFBuyYield "Failed")
