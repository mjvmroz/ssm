{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.SimpleStateMachine (
  StateMachine (..),
  MachineData (..),
  AnyState (..),
  transition,
  initBlind,
  transitionBlind,
) where

import Data.Data (Typeable, cast)
import Data.Kind (Type)

data AnyState machineTag where
  AnyState :: MachineData machineTag s -> AnyState machineTag

data MachineData machineTag stateTag where
  MachineData :: {props :: Props machineTag, state :: StateData machineTag stateTag} -> MachineData machineTag stateTag

{- | A state machine is a relationship between types that represent a finite state machine:
  - @m@ is the monadic context in which the state machine will be run
  - @machineTag@ is a type tag that identifies the state machine
  - @stateTag@ is a type tag that identifies the state of the state machine
  - @Props@ is a type that represents the immutable properties of the state machine (determined at initialization)
  - @StateData@ is a type that represents the state data of the state machine
    (computed at initialization and after each transition, corresponding to the current state tag)
  - @Init@ is a type that represents the initialization of the state machine
  - @Transition@ is a type that represents a transition of the state machine

  Initializations and transitions may fail in monadic contexts which allow it, and will return a @yield@,
  which is a type that represents the result of the operation.
  The expectation is that this will usually have some bearing on its environment, and that monadic executors wrapping
  this abstract definition will act on it.

  The monad constraint here is interesting. It's not really necessary, but it expresses the idea
  that the design of this relationship – which can in many cases be pure – is still conceptually
  dependent on the idea of a monadic context in which to act. State transitions have yields,
  and in more cases than not, those yields will have to be acted upon against some real world system,
  which might fail if only due to @IO@ being inherently fallible due to disease, famine, war, solar flares, etc.

  For the lucky rare few who enjoy the luxury of a pure context, consider the @Identity@ monad or hit me up to make your case.
-}
class (Monad m, Typeable machineTag, Typeable (StateTag machineTag)) => StateMachine m machineTag stateTag | machineTag -> stateTag where
  data Props machineTag :: Type
  data StateTag machineTag :: Type
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
  forall m machineTag st yield (s1 :: st) (s2 :: st).
  (StateMachine m machineTag st) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2, yield)
transition t d = transform <$> transitionState t d
 where
  transform (newState, yield) = (MachineData d.props newState, yield)

-- | Initialize a state machine. This should actually act on the yield, but for now we'll discard it.
initBlind ::
  forall m machineTag stateTag yield (s0 :: stateTag).
  (StateMachine m machineTag stateTag) =>
  Init machineTag s0 yield ->
  m (MachineData machineTag s0)
initBlind i = fst <$> initialize i

-- | Run a transition on a state machine. This should actually act on the yield, but for now we'll discard it.
transitionBlind ::
  forall m machineTag st yield (s1 :: st) (s2 :: st).
  (StateMachine m machineTag st) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2)
transitionBlind t d = fst <$> transition t d
