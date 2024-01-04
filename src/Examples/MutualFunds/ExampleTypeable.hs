module Examples.MutualFunds.ExampleTypeable where

import Data.Data (Typeable, cast)

data T = A | B | C deriving (Typeable, Show, Read)

data U (t :: T) where
  UA :: Int -> U 'A
  UB :: String -> U 'B
  UC :: Char -> U 'C

-- Define a sum type to represent any possible U
-- This existential enables us to hide the type parameter, and the @Typeable@ constraint enables us
-- to later attempt recovering it by testing the _type representation_ for equality with cast targets.
data AnyU = forall t. (Typeable t) => AnyU (U t)

-- Example function to attempt a cast to a specific subtype
attemptCast :: (Typeable t) => AnyU -> Maybe (U t)
attemptCast (AnyU u) = cast u

-- Usage example
-- >>> example
-- "It's a UA with value 4"
example :: String
example = do
  let unknownU = AnyU (UA 4) -- Imagine this comes from deserialization
  case attemptCast unknownU :: Maybe (U 'A) of
    Just (UA n) -> "It's a UA with value " ++ show n
    Nothing -> "idk it's not a UA that's for sure"
