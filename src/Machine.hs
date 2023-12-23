module Machine (
  StateMachine (..),
  MachineData (..),
  runInit,
  runTransition,
) where

import Data.Kind (Type)

data MachineData mt st where
  MachineData :: {props :: Props mt, state :: State mt st} -> MachineData mt st

class (Monad m) => StateMachine m (mt :: Type) (st :: Type) (yield :: Type) | mt -> st yield where
  data Props mt :: Type
  data State mt :: st -> Type
  data Transition mt :: st -> st -> Type
  data Init mt :: st -> Type

  initialize :: forall (s0 :: st). Init mt s0 -> m (MachineData mt s0, yield)

  transition :: forall (s1 :: st) (s2 :: st). Transition mt s1 s2 -> MachineData mt s1 -> m (State mt s2, yield)

-- | Initialize a state machine. This should actually act on the yield, but for now we'll discard it.
runInit ::
  forall (mt :: Type) (st :: Type) (yield :: Type) (m :: Type -> Type) (s0 :: st).
  (StateMachine m mt st yield) =>
  Init mt s0 ->
  m (MachineData mt s0)
runInit i = fst <$> initialize i

-- | Run a transition on a state machine. This should actually act on the yield, but for now we'll discard it.
runTransition ::
  forall (mt :: Type) (st :: Type) (yield :: Type) (m :: Type -> Type) (s1 :: st) (s2 :: st).
  (StateMachine m mt st yield) =>
  Transition mt s1 s2 ->
  MachineData mt s1 ->
  m (MachineData mt s2)
runTransition t d = MachineData d.props . fst <$> transition t d
