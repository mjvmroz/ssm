module Lib (
  someFunc,
) where

import Data.Kind (Type)

data MutualFund = MULSX | VUSXX deriving (Eq, Ord, Show)
type OrderId = Int
type Dollars = Int
type ShareUnits = Int

data MutualFundPurchase = PendingPurchase | ListedPurchase | ClosedPurchase | FailedPurchase deriving (Eq, Show)

data MutualFundPurchaseProps = VendingMachineProps
  { fund :: MutualFund
  , projectedUnits :: ShareUnits
  , dollars :: Dollars
  }

data MutualFundPurchaseState :: MutualFundPurchase -> Type where
  Pending :: MutualFundPurchaseState 'PendingPurchase
  Listed :: OrderId -> MutualFundPurchaseState 'ListedPurchase
  Closed :: MutualFundPurchaseState 'ClosedPurchase
  Failed :: MutualFundPurchaseState 'FailedPurchase

data MutualFundPurchaseTransition :: MutualFundPurchase -> MutualFundPurchase -> Type where
  List :: OrderId -> MutualFundPurchaseTransition 'PendingPurchase 'ListedPurchase
  Close :: Dollars -> ShareUnits -> MutualFundPurchaseTransition 'ListedPurchase 'ClosedPurchase
  Fail :: MutualFundPurchaseTransition 'PendingPurchase 'FailedPurchase

data MutualFundPurchaseInitialization :: MutualFundPurchase -> Type where
  InitPending :: MutualFundPurchaseProps -> MutualFundPurchaseInitialization 'PendingPurchase

data MutualFundPurchaseData (p :: MutualFundPurchase) = MutualFundPurchaseData
  { props :: MutualFundPurchaseProps
  , state :: MutualFundPurchaseState p
  }

initiateMFBuy :: MutualFundPurchaseInitialization s -> MutualFundPurchaseData s
initiateMFBuy (InitPending props) = MutualFundPurchaseData props Pending

transitionMFBuy :: MutualFundPurchaseTransition s1 s2 -> MutualFundPurchaseData s1 -> (MutualFundPurchaseState s2, String)
transitionMFBuy (List orderId) (MutualFundPurchaseData _ _) = (Listed orderId, "Listed")
transitionMFBuy (Close dollars units) (MutualFundPurchaseData props _) = (Closed, "Closed (" <> show props.fund <> " " <> show units <> " <-> $" <> show dollars <> ")")
transitionMFBuy Fail (MutualFundPurchaseData _ _) = (Failed, "Failed")

someFunc :: IO ()
someFunc = putStrLn "someFunc"
