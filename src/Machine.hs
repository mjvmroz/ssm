module Machine (
  StateMachine (..),
) where

import Data.Kind (Type)

class StateMachine (mt :: Type) (st :: Type) (yield :: Type) | mt -> st yield where
  data Props mt :: Type
  data State mt :: st -> Type
  data Transition mt :: st -> st -> Type
  data Init mt :: st -> Type

  initialize :: forall (s0 :: st). Init mt s0 -> (Props mt, State mt s0, yield)

  transition :: forall (s1 :: st) (s2 :: st). Transition mt s1 s2 -> Props mt -> State mt s1 -> (State mt s2, yield)
