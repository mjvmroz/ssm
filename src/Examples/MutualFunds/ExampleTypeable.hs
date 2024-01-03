module Examples.MutualFunds.ExampleTypeable where

import Data.Data

data T = A | B | C deriving (Typeable, Show, Read)

data U (t :: T) where
  UA :: Int -> U 'A
  UB :: String -> U 'B
  UC :: Char -> U 'C
  deriving (Typeable)

-- Define a sum type to represent any possible U
data AnyU = forall t. (Typeable t) => AnyU (U t)

-- Example function to attempt a cast to a specific subtype
attemptCast :: (Typeable t) => AnyU -> Maybe (U t)
attemptCast (AnyU u) = cast u

-- Usage example
-- >>> example
-- Just "It's a UA with value 5"
example :: Maybe String
example = do
  let unknownU = AnyU (UA 5) -- Imagine this comes from deserialization
  case attemptCast unknownU :: Maybe (U 'A) of
    Just (UA n) -> Just $ "It's a UA with value " ++ show n
    Nothing -> Nothing
