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

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: Contract w s Text Value             -- returns all Funds
ownFunds = do                                   -- combines values of all UTxOs
    pk    <- ownPubKey      --  look up our public key
    utxos <- utxoAt $ pubKeyAddress pk  -- get all the UTxOs at that public key address
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos -- UTxOs are a map from UTxO references to UTxOs
    -- Map.elems ignores the keys and gives us the values.
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just -- performs a monadic bind to the composite function tell . Last . Just which tells the value,
    void $ Contract.waitNSlots 1
    ownFunds'
