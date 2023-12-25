module Control.SimpleStateMachine (
  StateMachine (..),
  MachineData (..),
  initBlind,
  transitionBlind,
) where

import Data.Functor ((<&>))
import Data.Kind (Type)

data MachineData machineTag stateTag where
  MachineData :: {props :: Props machineTag, state :: StateData machineTag stateTag} -> MachineData machineTag stateTag

class (Monad m) => StateMachine m (machineTag :: Type) (stateTag :: Type) | machineTag -> stateTag where
  data Props machineTag :: Type
  data StateData machineTag :: stateTag -> Type
  data Init machineTag :: stateTag -> Type -> Type
  data Transition machineTag :: stateTag -> stateTag -> Type -> Type

  -- | Initialize a state machine, returning the initial @MachineData@ and a yield.
  initialize ::
    forall (s0 :: stateTag) yield.
    Init machineTag s0 yield ->
    m (MachineData machineTag s0, yield)

  -- | Run a transition on a state machine, returning the new @StateData@ and a yield.
  --   This is intended to be internal-only. Consumers should use @runTransition@.
  transitionState ::
    forall (s1 :: stateTag) (s2 :: stateTag) yield.
    Transition machineTag s1 s2 yield ->
    MachineData machineTag s1 ->
    m (StateData machineTag s2, yield)

transition ::
  forall (m :: Type -> Type) (machineTag :: Type) (st :: Type) (yield :: Type) (s1 :: st) (s2 :: st).
  (StateMachine m machineTag st) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2, yield)
transition t d = transform <$> transitionState t d
 where
  transform (newState, yield) = (MachineData d.props newState, yield)

-- | Initialize a state machine. This should actually act on the yield, but for now we'll discard it.
initBlind ::
  forall (m :: Type -> Type) (machineTag :: Type) (stateTag :: Type) (yield :: Type) (s0 :: stateTag).
  (StateMachine m machineTag stateTag) =>
  Init machineTag s0 yield ->
  m (MachineData machineTag s0)
initBlind i = fst <$> initialize i

-- | Run a transition on a state machine. This should actually act on the yield, but for now we'll discard it.
transitionBlind ::
  forall (m :: Type -> Type) (machineTag :: Type) (st :: Type) (yield :: Type) (s1 :: st) (s2 :: st).
  (StateMachine m machineTag st) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2)
transitionBlind t d = transition t d <&> fst
