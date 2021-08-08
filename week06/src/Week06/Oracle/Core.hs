{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol -- ^ Symbol of NFT, empty string as TokenName
    , oOperator :: !PubKeyHash     -- ^ Owner / Operator of Oracle (can do updates, gets fees)
    , oFee      :: !Integer        -- ^ Fees in lovelaces for data usage
    , oAsset    :: !AssetClass     -- ^ Exchange from ADA (e.g. USD)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use  -- Oracle Updates and Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName        -- Definition of the NFT asset Class.
oracleTokenName = TokenName emptyByteString -- use the empty string for the Token name

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass   -- oracleAsset is used to identify the NFT (different from oAsset)
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

-- oracleValue helper function --> takes an output transaction and a function which looks up the datum,
-- and then returns an Integer. The Integer represents the exchange rate (e.g. 1.75) multiplied by a million.
{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o -- txOutDatum can Fail because not every output has a datum.
                            -- If it Succeeds, we get a datum hash which we can reference in dh.
    Datum d <- f dh         --  f which is provided  to maybe turn this datum hash into a datum.
                            -- Can fail too. If it succeeds we can reference the result in d.
    PlutusTx.fromBuiltinData d

-- Oracle          -- ^ Parametrization, The datum
-- Integer         -- ^ Value, current exchange rate
-- OracleRedeemer  -- ^ Redeemer/Validator, 2 cases: use or update
-- In both cases (use & Update):  we want to check that we have the input that holds the NFT
-- and that there is an output that holds the NFT.

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  && -- checks if input holds NFT
    traceIfFalse "token missing from output" outputHasToken && -- checks if output holds the NFT
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) && -- check if the operator actually signed the transaction.
                  traceIfFalse "invalid output datum"       validOutputDatum -- check if Datum is at correct type
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                             -- Instead of checking only that it is an Integer,
                             -- here we also check that its output value is the same as the input value.
                  traceIfFalse "fees not paid"              feesPaid -- check that the fees have been paid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut  -- returns the TxOut that the script is trying to consume, which in this case is the oracle output.
    ownInput = case findOwnInput ctx of         -- Check that NFT exists exactly once in UTxO input
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i -- txInInfoResolved function gets the TxOut from the TxInInfo.

    inputHasToken :: Bool       -- checks that the token is present.
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1
    -- assetClassValueOf: look for the NFT within the ownInput response

    ownOutput :: TxOut                    -- Exists output which spents Oracle NFT
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool      -- checks that the token is present in output. Simliarly with above.
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer -- is used form validOutputDatum above
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    --feesPaid:  checks that the output value is at least as much as the input value plus the required fee.
    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle)) -- <> to add fee value to the input value

data Oracling
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

-- OFFCHAIN CODE
-- PARAMETERS
data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. OracleParams -> Contract w s Text Oracle    -- startOracle: responsibility is to mint the NFT
startOracle op = do                    -- will be used to idenify the oracle UTxO. No initial value provided.
                                       -- if we provided an initial value, it may be outdated by the time the NFT is minted.
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency) -- the error conversion function is provided  --> (pack . show)
                    -- show function converts the error to a String
                    -- pack function converts a String to a Data.Text type.
                    -- osc: holds the OneShotCurrency, so we can use the
                    -- currencySymbol function to get the currency symbol as cs.
    let cs     = Currency.currencySymbol osc -- NFT IS MINTED HERE and it has currency symbol cs
        oracle = Oracle         -- CONSTRUCT THE ORACLE Parameter Value
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do   -- Oracle started but no Initial Value
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c -- submit a transaction that produces the first value for the oracle.
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <> -- provide the lookup with one UTxO
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
                -- mustSpendScriptOutput is the opposite of mustPayToTheScript.
                -- Creates an input to this script address
                -- As parameters it takes the:
                -- reference to the UTxO we want to consume,
                -- the Redeemer which is Update and it is converted to the Plutus Data type.
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx -- transaction submitted using the Oracling type
            -- submitTxConstraintsWith: will send the fees to our own wallet
            awaitTxConfirmed $ txId ledgerTx        -- wait till transaction is confirmed
            logInfo @String $ "updated oracle value to " ++ show x      -- write log info

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer)) --  look up the existing oracle UTxO
              -- If it FAILS, means that we have jsut created the oracles and haven't created yet a UTxP with the Oracle Value
              -- If it SUCCEEDS,  return a triple containing
                -- 1) the UTxO identifer (TxOutRef),
                -- 2) the UTxO itself, which contains all the data (TxOutTx) and
                -- 3)the oracle value
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle) -- Map.filter --> returns True for the UTxO where the NFT is present
    return $ case Map.toList utxos of   -- a Map to all UTxOs which is either empty or contains one item.
        [(oref, o)] -> do
                    -- oracleValue: takes a TxOut parameter followed by a second parameter is a function,
                    -- which, given a datum hash will return the associated datum.
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
                    --  txData is a field of the transaction and it is a map from datum hashes to datums
            return (oref, o, x) -- SUCCESS CASE: x is the Integer Value of the oracle.
        _           -> Nothing -- When list is empty, we use _ element to represent all other cases.
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

-- OracleSchema: startOracle and updateOracle into one contract.
type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle -- write the oracle parameter (tell expects a Monoid type)
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
