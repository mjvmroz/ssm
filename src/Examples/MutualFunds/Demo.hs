module Examples.MutualFunds.Demo () where

import Control.SimpleStateMachine qualified as SSM
import Data.Functor.Identity (Identity (runIdentity))
import Examples.MutualFunds.Common
import Examples.MutualFunds.Process.Buy qualified as Buy

listed :: SSM.MachineData 'Buy Buy.Listed
listed = runIdentity $ SSM.initBlind (Buy.InitPending (Buy.Props MULSX 100 1000)) >>= SSM.transitionBlind (Buy.List 1)

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

dynamicClosed :: SSM.AnyMachineData 'Buy Buy.State
dynamicClosed = SSM.AnyMachineData closed

-- >>> closedViaDyn
-- Just MachineData 'Buy 'Closed {props = Props {fund = MULSX, projectedUnits = 100, dollars = 1000}, state = ClosedData}
closedViaDyn :: Maybe (SSM.MachineData Buy Buy.Closed)
closedViaDyn = dynamicClose dynamicListed

-- >>> notClosedIllegalState
-- Nothing
notClosedIllegalState :: Maybe (SSM.MachineData Buy Buy.Closed)
notClosedIllegalState = dynamicClose dynamicClosed -- 'Closed -> 'Closed is not valid
