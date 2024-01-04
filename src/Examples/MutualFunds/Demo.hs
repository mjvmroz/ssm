module Examples.MutualFunds.Demo where

import Control.SimpleStateMachine qualified as SSM
import Examples.MutualFunds.Common
import Examples.MutualFunds.Process.Buy qualified as Buy
import Data.Functor.Identity ( Identity(runIdentity) )

listed :: SSM.MachineData 'Buy Buy.Listed
listed = runIdentity do
  md1 <- SSM.initBlind (Buy.InitPending (Buy.Props MULSX 100 1000))
  SSM.transitionBlind (Buy.List 1) md1

dynamicListed :: SSM.AnyMachineData 'Buy Buy.State
dynamicListed = SSM.AnyMachineData listed

close :: SSM.MachineData 'Buy Buy.Listed -> SSM.MachineData 'Buy Buy.Closed
close = runIdentity . SSM.transitionBlind (Buy.Close 1000 100)

dynamicClose :: SSM.AnyMachineData 'Buy Buy.State -> Maybe (SSM.MachineData 'Buy Buy.Closed)
dynamicClose = runIdentity . SSM.dynamicTransitionBlind (Buy.Close 1000 100)

-- >>> closed
-- MachineData 'Buy 'Closed {props = Props {fund = MULSX, projectedUnits = 100, dollars = 1000}, state = ClosedData}
closed :: SSM.MachineData Buy Buy.Closed
closed = close listed

-- >>> closedViaDyn
-- Just MachineData 'Buy 'Closed {props = Props {fund = MULSX, projectedUnits = 100, dollars = 1000}, state = ClosedData}
closedViaDyn :: Maybe (SSM.MachineData Buy Buy.Closed)
closedViaDyn = dynamicClose dynamicListed

-- >>> notClosedIllegalState
-- Nothing
notClosedIllegalState :: Maybe (SSM.MachineData Buy Buy.Closed)
notClosedIllegalState = dynamicClose (SSM.AnyMachineData closed)
