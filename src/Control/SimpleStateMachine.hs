module Control.SimpleStateMachine (
  StateMachine (..),
  MachineData (..),
  runInit,
  runTransition,
) where

import Data.Kind (Type)

data MachineData machineTag stateTag where
  MachineData :: {props :: Props machineTag, state :: StateData machineTag stateTag} -> MachineData machineTag stateTag

class (Monad m) => StateMachine m (machineTag :: Type) (stateTag :: Type) (yield :: Type) | machineTag -> stateTag yield where
  data Props machineTag :: Type
  data StateData machineTag :: stateTag -> Type
  data Transition machineTag :: stateTag -> stateTag -> Type
  data Init machineTag :: stateTag -> Type

  -- | Initialize a state machine, returning the initial @MachineData@ and a yield.
  initialize ::
    forall (s0 :: stateTag).
    Init machineTag s0 ->
    m (MachineData machineTag s0, yield)

  -- | Run a transition on a state machine, returning the new @StateData@ and a yield.
  --   This is intended to be internal-only. Consumers should use @runTransition@.
  transition ::
    forall (s1 :: stateTag) (s2 :: stateTag).
    Transition machineTag s1 s2 ->
    MachineData machineTag s1 ->
    m (StateData machineTag s2, yield)

-- | Initialize a state machine. This should actually act on the yield, but for now we'll discard it.
runInit ::
  forall (m :: Type -> Type) (machineTag :: Type) (stateTag :: Type) (yield :: Type) (s0 :: stateTag).
  (StateMachine m machineTag stateTag yield) =>
  Init machineTag s0 ->
  m (MachineData machineTag s0)
runInit i = fst <$> initialize i

-- | Run a transition on a state machine. This should actually act on the yield, but for now we'll discard it.
runTransition ::
  forall (m :: Type -> Type) (machineTag :: Type) (st :: Type) (yield :: Type) (s1 :: st) (s2 :: st).
  (StateMachine m machineTag st yield) =>
  Transition machineTag s1 s2 ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2)
runTransition t d = MachineData d.props . fst <$> transition t d
