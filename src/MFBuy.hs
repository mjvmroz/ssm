module MFBuy where

import Data.Kind (Type)
import Machine

data MutualFund = MULSX | VUSXX deriving (Eq, Ord, Show)
type OrderId = Int
type Dollars = Int
type ShareUnits = Int

data MutualFundPurchase
data MutualFundPurchaseS = PendingPurchase | ListedPurchase | ClosedPurchase | FailedPurchase deriving (Eq, Show)

newtype MFBuyYield = MFBuyYield String deriving (Eq, Show)

instance StateMachine MutualFundPurchase MutualFundPurchaseS MFBuyYield where
  data Props MutualFundPurchase = VendingMachineProps
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
    (Props MutualFundPurchase, State MutualFundPurchase s0, MFBuyYield)
  initialize (InitPending props) = (props, Pending, MFBuyYield "Initialized with Pending State")

  transition ::
    forall (s1 :: MutualFundPurchaseS) (s2 :: MutualFundPurchaseS).
    Transition MutualFundPurchase s1 s2 ->
    Props MutualFundPurchase ->
    State MutualFundPurchase s1 ->
    (State MutualFundPurchase s2, MFBuyYield)
  transition (List orderId) _ _ = (Listed orderId, MFBuyYield "Listed")
  transition (Close dollars units) _ _ = (Closed, MFBuyYield ("Closed (" <> show units <> " <-> $" <> show dollars <> ")"))
  transition Fail _ _ = (Failed, MFBuyYield "Failed")
