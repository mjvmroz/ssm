{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.SimpleStateMachine (
  StateMachine (..),
  MachineData (..),
  AnyMachineData (..),
  transition,
  initBlind,
  transitionBlind,
  dynamicTransition,
  dynamicTransitionBlind,
) where

import Data.Data (Typeable, eqT, (:~:) (Refl))
import Data.Kind (Type)

{- Represents the current state of a machine of known type but dynamic state. -}
data AnyMachineData machineTag stateKind where
  AnyMachineData ::
    forall m machineTag stateKind {stateTag :: stateKind}.
    ( StateMachine m machineTag stateKind
    , Typeable machineTag
    , Typeable stateTag
    ) =>
    MachineData machineTag stateTag ->
    AnyMachineData machineTag stateKind
  deriving (Typeable)

{- | @MachineData@ represents the current state of a specific type and state.

  This is the most specific type we can use to represent machine state, and enables us to use the type system to
  enforce legal transitions.
-}
data MachineData machineTag stateTag where
  MachineData ::
    forall m machineTag stateKind (stateTag :: stateKind).
    (StateMachine m machineTag stateKind, Typeable machineTag, Typeable stateTag) =>
    { props :: Props machineTag
    , state :: StateData machineTag stateTag
    } ->
    MachineData machineTag stateTag

{- |
  A state machine is a relationship between types that represent a finite state machine:
  - @m@ constrains the monadic contexts in which the state machine can be run
  - @machineTag@ is a typelevel identifier that indicates the kind of machine
  - @stateKind@ is a typelevel identifier that indicates the state of a machine
  - @Props@ represents the immutable properties of the state machine (determined at initialization)
  - @StateData@ represents the state data of the state machine
    (computed at initialization and after each transition, corresponding to the current state tag)
  - @Init@ represents legal initializations of a state machine and associated yields
  - @Transition@ represents legal transitions of a state machine (including source and target states) and associated yields

  Initializations and transitions may fail in monadic contexts which allow it, and will return some @yield@,
  which is a type that represents the result of the operation. The expectation is that this will usually have
  some bearing on its environment, and that monadic executors wrapping this abstract definition will act on it
  to determine control flow or to derive other effects.

  The monad constraint here is interesting. It's not really necessary to _declare_ state machines, but it expresses
  an idea core to the design of this relationship: that we are conceptually dependent on the idea of a monadic context
  in which to act. State transitions are linear and must be run in a sensical sequence. They also have yields, and in
  more cases than not, those yields will have to be acted upon against some real world system, which might fail if
  only due to @IO@ being inherently fallible due to disease, famine, war, solar flares, etc.
-}
class (Monad m, Typeable machineTag) => StateMachine m machineTag stateKind | machineTag -> stateKind where
  data Props machineTag :: Type
  data StateData machineTag :: stateKind -> Type

  -- | state0 -> yield -> Type
  data Init machineTag :: stateKind -> Type -> Type

  -- | state1 -> state2 -> yield -> Type
  data Transition machineTag :: stateKind -> stateKind -> Type -> Type

  -- | Initialize a state machine, returning the initial @MachineData@ and a yield.
  initialize ::
    forall (s0 :: stateKind) yield.
    Init machineTag s0 yield ->
    m (MachineData machineTag s0, yield)

  -- | Run a transition on a state machine, returning the new @StateData@ and a yield.
  --   This is intended to be internal-only. Consumers should use @transition@.
  transitionState ::
    forall (s1 :: stateKind) (s2 :: stateKind) yield.
    Transition machineTag s1 s2 yield ->
    MachineData machineTag s1 ->
    m (StateData machineTag s2, yield)

transition ::
  forall m machineTag stateKind yield (s1 :: stateKind) (s2 :: stateKind).
  (StateMachine m machineTag stateKind, Typeable s2) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2, yield)
transition t d = transform <$> transitionState t d
 where
  transform (newState, yield) = (MachineData @m d.props newState, yield)

-- | Initialize a state machine. This should actually act on the yield, but for now we'll discard it.
initBlind ::
  forall m machineTag stateKind yield (s0 :: stateKind).
  (StateMachine m machineTag stateKind) =>
  Init machineTag s0 yield ->
  m (MachineData machineTag s0)
initBlind i = fst <$> initialize i

-- | Run a transition on a state machine. This should actually act on the yield, but for now we'll discard it.
transitionBlind ::
  forall m machineTag stateKind yield (s1 :: stateKind) (s2 :: stateKind).
  (StateMachine m machineTag stateKind, Typeable s2) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2)
transitionBlind t d = fst <$> transition t d

dynamicTransition ::
  forall m machineKind {machineTag :: machineKind} stateKind {expectedState :: stateKind} {targetState :: stateKind} yield.
  (StateMachine m machineTag stateKind, Typeable expectedState, Typeable targetState) =>
  Transition machineTag expectedState targetState yield ->
  AnyMachineData machineTag stateKind ->
  m (Maybe (MachineData machineTag targetState, yield))
dynamicTransition t (AnyMachineData (MachineData props actualState :: MachineData machineTag actualState)) =
  case eqT @actualState @expectedState of
    Just Refl -> Just <$> transition t (MachineData @m props actualState)
    Nothing -> pure Nothing

dynamicTransitionBlind ::
  forall m machineKind {machineTag :: machineKind} stateKind {expectedState :: stateKind} {targetState :: stateKind} yield.
  (StateMachine m machineTag stateKind, Typeable expectedState, Typeable targetState) =>
  Transition machineTag expectedState targetState yield ->
  AnyMachineData machineTag stateKind ->
  m (Maybe (MachineData machineTag targetState))
dynamicTransitionBlind t amd =
  fmap fst <$> dynamicTransition t amd