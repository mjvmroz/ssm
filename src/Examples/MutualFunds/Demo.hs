module Examples.MutualFunds.Demo where

import Control.SimpleStateMachine qualified as SSM
import Examples.MutualFunds.Buy qualified as Buy
import Examples.MutualFunds.Common (MutualFund (MULSX))

example :: (SSM.StateMachine m Buy.MutualFundPurchase Buy.MutualFundPurchaseState) => m (SSM.MachineData Buy.MutualFundPurchase Buy.Closed)
example = do
  md1 <- SSM.initBlind (Buy.InitPending (Buy.Props MULSX 100 1000))
  md2 <- SSM.transitionBlind (Buy.List 1) md1
  SSM.transitionBlind (Buy.Close 1000 100) md2

-- deserializeBuy :: (SSM.StateMachine m Buy.MutualFundPurchase Buy.MutualFundPurchaseState) => String -> SSM.MachineData Buy.MutualFundPurchase Buy.MutualFundPurchaseState
-- deserializeBuy s = case s of
--   "pending" -> SSM.MachineData (Buy.Props MULSX 100 1000) Buy.PendingData
--   _ -> error "invalid state"

tryClose ::
  (SSM.StateMachine m Buy.MutualFundPurchase Buy.MutualFundPurchaseState) =>
  SSM.AnyState Buy.MutualFundPurchase ->
  m (Maybe (SSM.MachineData Buy.MutualFundPurchase Buy.Closed))
tryClose (SSM.AnyState md) = case md of
  listedData@(SSM.MachineData _ (Buy.ListedData _)) -> Just <$> SSM.transitionBlind (Buy.Close 1000 100) listedData
  _ -> pure Nothing