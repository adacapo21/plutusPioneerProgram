{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- EmulatorTrace a

-- we pass a Contract constructed with Unit as the w type and BlockchainActions as the second argument, s. This gives us access to all the blockchain actions
-- the only thing we can’t do is to call specific endpoints.
myContract1 :: Contract () Empty Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

-- define a Trace that starts the contract in the wallet
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

-- test function to run the trace
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

--The operator .\/ is a type operator - it operates on types, not values.
-- In order to use this we need to use the TypeOperators and DataKinds compiler options.
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text () -- MySchema Type define our contract
myContract3 = do
    n <- endpoint @"foo" -- This contract will block until the endpoint foo is called with, in our case, an Int
    Contract.logInfo n
    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
