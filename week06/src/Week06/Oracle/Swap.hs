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

module Week06.Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           Week06.Oracle.Core
import           Week06.Oracle.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs && -- check if the seller gets paid
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut --  to get the UTxO from the oracle.
    oracleInput =
      let                -- get list of all inputs, then it compares with the addr parameter.
                         -- the list will be EMPTY or it will have the TxOut that matches the oracle UTxO.
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of      -- we check if there is only one element in the list (NFT)
            [o] -> o     -- if it EXISTS, returns the TxOut.
            _   -> traceError "expected exactly one oracle input"

    -- Check the actual EXCHANGE RATE
    -- If it succeeds returns the value
    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
        -- Filter by doing  1) Get the UTxO from the input 2) get the address for this UTxO 3)the validator hash for that address.
        -- Checking if it is a script output by seeing if it is a Just
        -- If it is a Nothing, then this would show that it is a public key, NOT a script address.
      in
        length xs == 2 -- check length of list is exactly 2

    minPrice :: Integer -- DEFINE THE REQUIRED PRICE
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of  -- CHECK IF INPUT, ASSIGN that number to lovelaceIn
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        -- use the price helper function to determine the price in USD tokens.
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
        --  valuePaidTo is from the Plutus libraries, Given info and a public key hash, it will add up all the values of all the public key outputs that go to this address.
        -- assetClassValueOf: check the component of the value that is in USD token,
            -- and the check that we have at least as many as we require.
      in
        pricePaid >= minPrice
    -- LAST LINE ABOVE FOR main part of the code THE SWAP VALIDATOR

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

-- Takes Oracle to use and lovelaces seller wants to add
-- Pays amout of lovelaces to script (swap)
offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

-- find all swaps that satisfy a given predicate
-- takes an oracle plus a predicate based on public key hashes,
-- and returns a list of triples of the UTxOs that satisfy the predicate.
findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    -- mapMaybe: will apply the (a -> Maybe b) function to each element in a list of as
    -- and creates a list of Maybe bs
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        PlutusTx.fromBuiltinData d

    -- use the mapMaybe and the function g to filter the list of UTxOs.
    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o  -- gets the public key hash from a UTxO, if it exists.
        guard $ p pkh   -- function g uses the guard function with the predicate function p that we passed in as an argument.
        return (oref, o, pkh)

-- retrieveSwaps contract is for the seller
-- if they want to change their mind and get their Ada back
retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
                -- extracting a list of orefs from the xs list and using it to construct a constraint for each of them, using Unit as the Redeemer type.
                -- Function mconcat applies the Semigroup operator <> throughout the list in order to combine them.
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

-- where the ORACLE is used
useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds -- add up all the money in our own wallet and returns a Value.
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt -- find out how many USD Tokens we have.

    m <- findOracle oracle -- it finds us the oracle UTxO that contains the oracle value.
    case m of
        Nothing           -> logInfo @String "oracle not found" -- FAIL CASE OF FINDING THE ORACLE
        Just (oref, o, x) -> do -- CASE OF FINDING THE ORACLE
            logInfo @String $ "found oracle, exchange rate " ++ show x -- log message with current exchange rate
            pkh   <- pubKeyHash <$> Contract.ownPubKey -- check our own public key
            swaps <- findSwaps oracle (/= pkh)         -- check for all available swaps where we are not the owner
            case find (f amt x) swaps of    -- find returns a list that satisfies the predicate in the condition
                        -- f is defined below, in the predicate after "where"
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle) -- construct a transaction/ output for the Oracle which includes any fees + fee in lovelace that we need to pay
                        -- p: create a Value representing the USD Tokens that we need to pay.
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        -- CONSTRAINTS
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>     -- we must provide the two UTxOs that we want to consume.
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <> -- consume the oracle as an input, first use of the Use redeemer.
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  -- second constraint is to consume the swap input, which just uses a Unit redeeemer.
                                  Constraints.mustPayToOtherScript         -- Third constraint is to pay the oracle.
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                    --  final constraint is that we must pay the seller of the lovelace
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx -- SUBMIT
                    awaitTxConfirmed $ txId ledgerTx    -- WAIT FOR CONFIRMATION
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p) -- LOG MESSAGE
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    -- f function: The function determines if there is a swap that is cheaper to or equal to the amount parameter.
    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

-- A Bundle that contains ALL OF THEM
type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

-- The swap function recursively calls itself, offering again and again the same choice of endpoints.
swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer" -- we block until we are provided with an amt and
        offerSwap oracle amt     -- then we call the offerSwap contract.

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
