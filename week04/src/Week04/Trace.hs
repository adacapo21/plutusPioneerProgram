{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints    -- ind the result of monadic function activateContractWallet to h1
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams                 -- to simulate Wallet 1 calling the give endpoint,
                                                         -- with the shown parameters.
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20                               -- returns a value representing the slot that was reached
    callEndpoint @"grab" h2 ()                            -- simulate the call to the grab endpoint by Wallet 2
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s
