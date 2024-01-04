module Examples.MutualFunds.Unified where

import Control.SimpleStateMachine qualified as SSM
import Data.Kind ( Type )
import Data.Map.Strict qualified as Map
import Examples.MutualFunds.Common ( InvestmentProcess(..) )

data InvestmentProcessId (p :: InvestmentProcess) id where
  BuyId :: id -> InvestmentProcessId 'Buy id
  SellId :: id -> InvestmentProcessId 'Sell id

data InvestmentProcessData (p :: InvestmentProcess) :: Type where
  BuyData :: SSM.AnyMachineData 'Buy -> InvestmentProcessData 'Buy
  SellData :: SSM.AnyMachineData 'Sell -> InvestmentProcessData 'Sell

data AnyInvestmentProcessData where
  AnyInvestmentProcessData :: InvestmentProcessData p -> AnyInvestmentProcessData

data InvestmentProcessPool id = (Ord id) => InvestmentProcessPool
  { buys :: Map.Map id (SSM.AnyMachineData 'Buy)
  , sells :: Map.Map id (SSM.AnyMachineData 'Sell)
  }

instance Semigroup (InvestmentProcessPool id) where
  (<>) :: InvestmentProcessPool id -> InvestmentProcessPool id -> InvestmentProcessPool id
  InvestmentProcessPool { buys = buys1, sells = sells1 }
    <> InvestmentProcessPool { buys = buys2, sells = sells2 }
    = InvestmentProcessPool
      { buys = buys1 <> buys2
      , sells = sells1 <> sells2
      }

instance Ord id => Monoid (InvestmentProcessPool id) where
  mempty :: InvestmentProcessPool id
  mempty = InvestmentProcessPool { buys = Map.empty, sells = Map.empty }

groupByProcessType :: forall id. Ord id => Map.Map id AnyInvestmentProcessData -> InvestmentProcessPool id
groupByProcessType = Map.foldrWithKey go mempty
  where
    go :: id -> AnyInvestmentProcessData -> InvestmentProcessPool id -> InvestmentProcessPool id
    go processId (AnyInvestmentProcessData (BuyData md)) pool = pool { buys = Map.insert processId md pool.buys }
    go processId (AnyInvestmentProcessData (SellData md)) pool = pool { sells = Map.insert processId md pool.sells }

resolveById :: Ord id => InvestmentProcessId p id -> InvestmentProcessPool id -> Maybe (SSM.AnyMachineData p)
resolveById (BuyId pId) pool = Map.lookup pId pool.buys
resolveById (SellId pId) pool = Map.lookup pId pool.sells
