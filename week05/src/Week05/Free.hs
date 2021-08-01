{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
-- ON CHAIN SCRIPT
-- SIMPLE MINTING POLICY
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool -- Definition of mkPolicy
                                        -- Unlike validators, minting policies only accept a redeemer and context.
mkPolicy () _ = True                    -- returns True unconditionally.

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||]) -- where mkPolicy is compiled
                                        -- Once mkPolicy has been defined,
                                        -- it can then be compiled into Plutus script using mkMintingPolicyScript

curSymbol :: CurrencySymbol             -- A minting policy can be transformed into a CurrencySymbol hash
curSymbol = scriptCurrencySymbol policy -- using the scriptCurrencySymbol utility

-- OFFCHAIN SCRIPT 
data MintParams = MintParams
    { mpTokenName :: !TokenName                         --
    , mpAmount    :: !Integer                           -- if mpAmount > 0 --> CREATE TOKENS
                                                        -- if mpAmount < 0 --> BURN TOKENS
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams            -- DEFINE SCHEMA

mint :: MintParams -> Contract w FreeSchema Text ()     -- CREATE THE ENDPOINT
                                                        -- WITH 2 MISSING ARGUMENTS (TokenName / Amount)
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)  -- DEFINE VALUE THAT WE WANT TO MINT
                            -- ARGUMENTS:
                            -- the currency symbol that represents the hash of the minting policy
                            -- Token name
                            -- Amount extracted from the MintParams
        lookups = Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValue val             -- it must mint the previous defined value : val
                                                            -- It gives a transaction that we can send to the chain
    ledgerTx <- submitTxConstraintsWith @Void lookups tx    -- HOW the transaction is sent to the chain --> submitTxConstraintsWith
    void $ awaitTxConfirmed $ txId ledgerTx                 -- a handle to the transaction
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()                 -- DEFINE ENDPOINTS TO EXECUTE mint FUNCTION
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
