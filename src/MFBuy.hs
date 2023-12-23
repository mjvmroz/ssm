module MFBuy (
  MutualFundPurchase,
  MutualFundPurchaseS,
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
data MutualFundPurchaseS = PendingPurchase | ListedPurchase | ClosedPurchase | FailedPurchase deriving (Eq, Show)

newtype MFBuyYield = MFBuyYield String deriving (Eq, Show)

instance (Monad m) => StateMachine m MutualFundPurchase MutualFundPurchaseS MFBuyYield where
  data Props MutualFundPurchase = Props
    { fund :: MutualFund
    , projectedUnits :: ShareUnits
    , dollars :: Dollars
    }

  data State MutualFundPurchase :: MutualFundPurchaseS -> Type where
    Pending :: State MutualFundPurchase 'PendingPurchase
    Listed :: OrderId -> State MutualFundPurchase 'ListedPurchase
    Closed :: State MutualFundPurchase 'ClosedPurchase
    Failed :: State MutualFundPurchase 'FailedPurchase

  data Transition MutualFundPurchase :: MutualFundPurchaseS -> MutualFundPurchaseS -> Type where
    List :: OrderId -> Transition MutualFundPurchase 'PendingPurchase 'ListedPurchase
    Close :: Dollars -> ShareUnits -> Transition MutualFundPurchase 'ListedPurchase 'ClosedPurchase
    Fail :: Transition MutualFundPurchase 'PendingPurchase 'FailedPurchase

  data Init MutualFundPurchase :: MutualFundPurchaseS -> Type where
    InitPending :: Props MutualFundPurchase -> Init MutualFundPurchase 'PendingPurchase

  initialize ::
    forall (s0 :: MutualFundPurchaseS).
    Init MutualFundPurchase s0 ->
    m (MachineData MutualFundPurchase s0, MFBuyYield)
  initialize (InitPending props) = pure (MachineData props Pending, MFBuyYield "Initialized with Pending State")

  transition ::
    forall (s1 :: MutualFundPurchaseS) (s2 :: MutualFundPurchaseS).
    Transition MutualFundPurchase s1 s2 ->
    MachineData MutualFundPurchase s1 ->
    m (State MutualFundPurchase s2, MFBuyYield)
  transition (List orderId) _ = pure (Listed orderId, MFBuyYield "Listed")
  transition (Close dollars units) _ = pure (Closed, MFBuyYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transition Fail _ = pure (Failed, MFBuyYield "Failed")
