module Examples.MutualFunds.Demo where

import Control.SimpleStateMachine qualified as SSM
import Examples.MutualFunds.Common
import Examples.MutualFunds.Process.Buy qualified as Buy

listed :: (SSM.StateMachine m 'Buy Buy.State) => m (SSM.MachineData 'Buy Buy.Listed)
listed = do
  md1 <- SSM.initBlind (Buy.InitPending (Buy.Props MULSX 100 1000))
  SSM.transitionBlind (Buy.List 1) md1

closed :: (SSM.StateMachine m 'Buy Buy.State) => m (SSM.MachineData 'Buy Buy.Closed)
closed = listed >>= SSM.transitionBlind (Buy.Close 1000 100)

-- x :: SSM.AnyMachineData 'Buy Buy.State
-- x = SSM.AnyMachineData listed

-- >>> SSM.AnyMachineData listed >>= tryClose
-- Couldn't match type `AnyMachineData 'Buy State' with `Type'
-- Expected: Type
--           -> AnyMachineData
--                machineTag_ae24[sk:1] (Maybe (MachineData 'Buy 'Closed))
--   Actual: AnyMachineData 'Buy State
--           -> AnyMachineData
--                machineTag_ae24[sk:1] (Maybe (MachineData 'Buy 'Closed))
-- In the second argument of `(>>=)', namely `tryClose'
-- In the expression: AnyMachineData listed >>= tryClose
-- In an equation for `it_adZV':
--     it_adZV = AnyMachineData listed >>= tryClose
tryClose ::
  (SSM.StateMachine m 'Buy Buy.State) =>
  SSM.AnyMachineData 'Buy Buy.State ->
  m (Maybe (SSM.MachineData 'Buy Buy.Closed))
tryClose = SSM.dynamicTransitionBlind (Buy.Close 1000 100)
