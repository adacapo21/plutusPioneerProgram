{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Week06.Oracle.Core                  as Oracle
import           Week06.Oracle.PAB                   (OracleContracts (..))
import qualified Week06.Oracle.Swap                  as Oracle

-- ENTRY POINT OF EXECUTABLE
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."        -- Simulator monad is very similar to the EmulatorTrace monad
                                                                                                                -- HERE: log that we are starting the PAB server
    shutdown <- PAB.Server.startServerDebug                                 -- the return value of that function which gets bound to shutdown
                                                                            -- can be used later to shut down the server.

    cidInit <- Simulator.activateContract (Wallet 1) Init                   -- It takes a wallet where we want to start that instance,
                                                                            -- and then a value of the reified contract type
    cs      <- waitForLast cidInit                                          -- the currency symbol
    _       <- Simulator.waitUntilFinished cidInit                          -- Wait until initContract has finished

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs          -- start the oracle on Wallet 1
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle -- cidOracle --> Interact with the oracle from the outside world
    oracle <- waitForLast cidOracle                                         -- get the Oracle value
    -- THE NFT IS MINTED & WE know the Oracle Value

    forM_ wallets $ \w ->                                                   -- loop over all the wallets
        when (w /= Wallet 1) $ do                                           -- except of the wallet 1 which runs the oracle
            cid <- Simulator.activateContract w $ Swap oracle               -- activation of Swap contract on each of them
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine              -- when enter is pressed then
    shutdown                           -- close server

-- BELOW THE IDEA IS:
-- it will read the state of the contract which we wrote using tell. --> JSON value
-- it applies this JSON value to the provided predicate
-- If the result is Nothing, it simply waits until the state changes
-- if it is Just x, it will return the x.
-- the function waits until the state of the contract has told a Just value.
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a    --  takes a contract instance and a predicate.
waitForLast cid =                                                              -- The predicate gets a JSON expression and returns a Maybe a.
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init     -> endpointsToSchemas @Empty                           -- Init won't have any schemm
        Oracle _ -> endpointsToSchemas @Oracle.OracleSchema             -- Oracle uses OracleSchema
        Swap _   -> endpointsToSchemas @Oracle.SwapSchema               -- Swap uses SwapSchema
    getContract = \case
        Init        -> SomeBuiltin   initContract                       -- Init will run the initCOntract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs -- Oracle will run the runOracle contract with oracleParams
                                                                        -- which takes the currency symbol of the USD Token and defines example oracle params.
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle                 -- Swap will run our swap contract with an oracle value

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) def []
    $ interpret handleOracleContracts

initContract :: Contract (Last CurrencySymbol) Empty Text ()            -- initContract function mints USD Tokens and distributes them to the wallets,
initContract = do                                                       -- then it tells the currency symbol for the USD Token.
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur
        v  = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000
