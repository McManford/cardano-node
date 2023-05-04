{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    -- * Simplest query related
    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)
import           Data.Bifunctor (first)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api.Certificate
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.AnyQuery
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Query.ShelleyBased
import           Cardano.Api.TxBody

data QueryConvenienceError
  = SockErr EnvSocketError
  | QueryConvenienceError AllQueryErrors

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (SockErr e) = renderEnvSocketError e
renderQueryConvenienceError (QueryConvenienceError e) = Text.pack $ show e

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: NetworkId
  -> [TxIn]
  -> IO (Either QueryConvenienceError (AnyUTxO, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx networkId allTxIns = runExceptT $ do
  SocketPath sockPath <- newExceptT $ first SockErr <$> readEnvSocketPath
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo
                            cModeParams
                            networkId
                            sockPath
  firstExceptT QueryConvenienceError $ newExceptT
    $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
        AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
        eInMode <- determineEraInModeAnyQuery era cModeParams
        case cardanoEraStyle era of
          LegacyByronEra -> left AllQueryEraExpectedSbe
          ShelleyBasedEra sbe -> do
            let utxoQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe
                              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
                pparamsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                 $ QueryInShelleyBasedEra sbe QueryProtocolParameters
                eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                systemStartQuery = AnyQueryAnyEra QuerySystemStart
                stakePoolsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                    $ QueryInShelleyBasedEra sbe QueryStakePools
            utxo <- AnyUTxO (shelleyBasedToCardanoEra sbe) <$> queryExprAnyQueryE utxoQuery
            pparams <- queryExprAnyQueryE pparamsQuery
            eraHistory <- queryExprAnyQuery eraHistoryQuery
            systemStart <- queryExprAnyQuery systemStartQuery
            stakePools <- queryExprAnyQueryE stakePoolsQuery
            return (utxo, pparams, eraHistory, systemStart, stakePools)



