{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.SimpleStateMachine (
  StateMachine (..),
  MachineData (..),
  AnyMachineData (..),
  transition,
  initBlind,
  transitionBlind,
  attemptDynamicTransition_,
  dynamicTransition,
) where

import Data.Kind (Type)
import Data.Data ( Typeable, eqT, type (:~:)(Refl), cast )
import Type.Reflection

{- Represents the current state of a machine of known type but dynamic state. -}
data AnyMachineData machineTag where
  AnyMachineData :: (
    StateMachine m machineTag stateTag,
    Typeable machineTag,
    Typeable stateTag,
    Typeable (StateData machineTag stateTag),
    Typeable (Props machineTag),
    Typeable (MachineData machineTag)
    ) => MachineData machineTag stateTag -> AnyMachineData machineTag

{- | @MachineData@ represents the current state of a specific type and state.

  This is the most specific type we can use to represent machine state, and enables us to use the type system to
  enforce legal transitions.
-}
data MachineData machineTag stateTag where
  MachineData ::
    { props :: Props machineTag
    , state :: StateData machineTag stateTag
    } -> MachineData machineTag stateTag

{- |
  A state machine is a relationship between types that represent a finite state machine:
  - @m@ constrains the monadic contexts in which the state machine can be run
  - @machineTag@ is a typelevel identifier that indicates the kind of machine
  - @stateTag@ is a typelevel identifier that indicates the state of a machine
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
class (Monad m) => StateMachine m machineTag stateTag | machineTag -> stateTag where
  data Props machineTag :: Type
  data StateData machineTag :: stateTag -> Type
  -- | state0 -> yield -> Type
  data Init machineTag :: stateTag -> Type -> Type
  -- | state1 -> state2 -> yield -> Type
  data Transition machineTag :: stateTag -> stateTag -> Type -> Type

  -- | Initialize a state machine, returning the initial @MachineData@ and a yield.
  initialize ::
    forall (s0 :: stateTag) yield.
    Init machineTag s0 yield ->
    m (MachineData machineTag s0, yield)

  -- | Run a transition on a state machine, returning the new @StateData@ and a yield.
  --   This is intended to be internal-only. Consumers should use @transition@.
  transitionState ::
    forall (s1 :: stateTag) (s2 :: stateTag) yield.
    Transition machineTag s1 s2 yield ->
    MachineData machineTag s1 ->
    m (StateData machineTag s2, yield)

transition ::
  forall m machineTag stateTag yield (s1 :: stateTag) (s2 :: stateTag).
  (StateMachine m machineTag stateTag) =>
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
  forall m machineTag stateTag yield (s1 :: stateTag) (s2 :: stateTag).
  (StateMachine m machineTag stateTag) =>
  Transition machineTag s1 s2 yield ->
  MachineData machineTag s1 ->
  m (MachineData machineTag s2)
transitionBlind t d = fst <$> transition t d

-- | Not very useful. I really need an equivalent for the existential (AnyMachineData) for dynamic applications.
attemptDynamicTransition_ ::
  forall m machineTag stateTag (actualState :: stateTag) (expectedState :: stateTag) (targetState :: stateTag) yield.
  (StateMachine m machineTag stateTag, Typeable actualState, Typeable expectedState) =>
  Transition machineTag expectedState targetState yield ->
  MachineData machineTag actualState ->
  m (Maybe (MachineData machineTag targetState, yield))
attemptDynamicTransition_ t (MachineData props actualState) =
  case eqT @actualState @expectedState of
    Just Refl -> Just <$> transition t (MachineData props actualState)
    Nothing -> pure Nothing

dynamicTransition ::
  forall m machineTag stateTag (expectedState :: stateTag) (targetState :: stateTag) yield.
  (StateMachine m machineTag stateTag, Typeable expectedState) =>
  Transition machineTag expectedState targetState yield ->
  AnyMachineData machineTag ->
  m (Maybe (MachineData machineTag targetState, yield))
dynamicTransition t (AnyMachineData (MachineData props actualState)) =
  dynamicTransition' actualState
    where
      dynamicTransition' ::
        forall (actualState :: stateTag). (Typeable actualState) =>
        StateData machineTag actualState -> m (Maybe (MachineData machineTag targetState, yield))
      dynamicTransition' s = case eqT @actualState @expectedState of
        Just Refl -> Just <$> transition t (MachineData props s)
        Nothing -> pure Nothing
