module Examples.MutualFunds.Demo where

import Control.SimpleStateMachine qualified as SSM
import Examples.MutualFunds.Common
import Examples.MutualFunds.Process.Buy qualified as Buy

example :: (SSM.StateMachine m 'Buy Buy.State) => m (SSM.MachineData 'Buy Buy.Closed)
example = do
  md1 <- SSM.initBlind (Buy.InitPending (Buy.Props MULSX 100 1000))
  md2 <- SSM.transitionBlind (Buy.List 1) md1
  SSM.transitionBlind (Buy.Close 1000 100) md2

tryClose ::
  (SSM.StateMachine m 'Buy Buy.State) =>
  SSM.AnyMachineData 'Buy Buy.State ->
  m (Maybe (SSM.MachineData 'Buy Buy.Closed))
tryClose = SSM.dynamicTransitionBlind (Buy.Close 1000 100)