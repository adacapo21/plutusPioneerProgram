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

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game                        -- data type Game - a parameter for the contract
    { gFirst          :: !PubKeyHash    -- players identified by their hashes
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer       -- number of lovelace to be used as stake (nust be provided by each player)
    , gPlayDeadline   :: !POSIXTime     -- the deadline slot by which the second player must make their move
    , gRevealDeadline :: !POSIXTime     -- the slot by which player 1 must claim victory by revealing his nonce.
    , gToken          :: !AssetClass    -- an arbitrary NFT, used to identify the right instance of the UTxO that we are using.
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

-- DEFINITIONS OF MOVES THAT PLAYERS CAN DO
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice
-- FOR STATE WE USE THE BELOW TYPE
data GameDatum = GameDatum ByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

-- REDEEMER - USING A CUSTOM TYPE
-- Play: where the second player moves
-- Reveal:  is for the case where the fisrts player has won - how they will reveal their nonce which is the ByteString
-- ClaimFirst: when 1st player claims back the stake when the 2nd doesn't move by the gPlayDeadline
-- ClaimSecond: when the 1st player doesn't reveal by the gRevealDeadline
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer       -- helper function: gets number of lovelaces Held in Value
lovelaces = Ada.getLovelace . Ada.fromValue

-- gameDatum behaves similarly with oracleValue
-- oracleValue helper function --> takes an output transaction and a function which looks up the datum,
-- and then returns an Integer.
{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- CORE BUSINESS LOGIC
-- game
-- bsZero' bsOne': Used as somewhat of nuisance. USe them as string literals to get ByteStrings (represent 0 adn 1 respectively)
-- dat: Datum
-- red: Redeemer
-- ctx: Context
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    -- CONDITION thas covers all the cases, and
    -- that is that the INPUT we are validating MUST CONTAIN the NFT.
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of      -- RULES DEPEND ON THE SITUATION (every situation explained in the next lines)
        -- 2nd player is moving (has not moved yet)
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))    -- 2nd player signed the trasnaction                               &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&  -- 1st player’s stake is present in the input
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&  -- output should have the 2nd player’s stake added to the total stake
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&  -- Hash must be the same & the move made by the second player
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&  -- Deadline
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)          -- the NFT must be passed on in the output UTxO.

        -- 1st player has won, reveals his nonce in order to claim
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)     -- Nonce should agree with the hash submitted earlier                                       &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- 2nd player has not moved before `gPlayDeadline`,
        -- so 1st can claim back money
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- Both have moved, 1st has lost or not revealed in time, so 2nd can claim stake
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst
        -- FOUR CASES ONLY HERE, THE REST WILL FAIL
        _                              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut       -- should never fail, as we are in the process of validating a UTxO
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    outputDatum :: GameDatum    -- makes use of the GameDatum type
    outputDatum = case gameDatum ownOutput (`findDatum` info) of -- EXACTLY ONE output(the return from ownOutput), it will give us the datum.
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- function which is used when the 1st player has won and wants to prove it
    -- by revealing their nonce
    -- cFirst & cSecond difference: one is GameChoice and is ByteString --> They can be swapped!
    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool -- the
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs
      where
        cFirst :: ByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    -- NFT goes back to the first player:
    -- when the game is over and there is NO ADDRESS anymore
    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1

-- ONCHAIN CODE
data Gaming     -- DEFINE DATA TYPE THAT HOLDS THE INFO
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: ByteString     -- Definitions of Bytestrings that will be used for 2 choices
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game     -- apply 3 parameter
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer
-- VALIDATORS
gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator
-- ADDRESS
gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- Finds correct UTxO with token and game state
-- the one that carries the NFT
-- Contract: used to find the UTxO with the NFT (returns Maybe || anything)
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos -- get list of UTxOs at the game address
                     -- f function: checks whether the output contains the NFT
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

-- Params for game initialization, 1st player is the owner of the wallet, so we know their public key hash
data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash -- hash of 2nd player
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString   -- nonce
    , fpCurrency       :: !CurrencySymbol -- NFT (split into currency and Token Name)
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice   -- move of player
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Contract for first player
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey    -- get the public key hash
    let game = Game    -- we populate the fields of the game.
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1 -- stake + NFT which both go to UTxO
        c    = fpChoice fp      --  calculate the hash that we need to send as our disguised move
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx    -- SUBMIT Transaction
                -- CONSTRAINTS: a UTxO with the datum of the move & value v
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    waitUntilTimeHasPassed $ fpPlayDeadline fp  -- wait the deadline slot, at which point the winner can be determined.

    m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"   -- UTxO is not found, somehting is wrong.
        Just (oref, o, dat) -> case dat of
            GameDatum _ Nothing -> do       -- UTxO FOUND but 2nd hasn't moved --> ClaimFirst redeemer to get the stake back
                -- 1st case: the second player hasn’t moved.
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                -- second case: second player did move, and they lost
                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000) -- transaction must be submitted before the reveal deadline has passed.
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"
                -- 3rd case: second player played and won, there is nothing for use to do.
            _ -> logInfo @String "second player played and won"

-- Contract for 2nd player
-- Similar to 'FirstParams', BUT:
-- NO NEED of the second player’s public key hash
-- NO NEED of nonce required
data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey -- get our own public key hash
    let game = Game     -- set up the game values
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game -- try to find the UTxO that contains the NFT
    case m of  -- NFT FOUND THEN:
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1 -- ASSIGN the NFT to TOKEN.
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token -- calculate the value that we must put in the new output
                        -- CONSUME EXISTING UTXO && create new one at the same address
                        -- first will contain the stake that 1st player added
                        -- add our own stake
                        -- keep the NFT in there
                c       = spChoice sp   -- our choice
                -- LOOKUPS
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                -- CONSTRAINTS
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <> -- CONSUME EXISTING UTXO WITH Play redeemer with our choice
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <> -- CREATE A NEW UTXO WITH THE UPDATED DATUM
                          Constraints.mustValidateIn (to now)       -- must be done before Deadline passes
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx         -- SUBMIT
            void $ awaitTxConfirmed tid     -- confirmation
            logInfo @String $ "made second move: " ++ show (spChoice sp) -- LOG

            waitUntilTimeHasPassed $ spRevealDeadline sp -- wait until the reveal deadline has passed.

            -- find the UTxO, which could now be a different one.
            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won" -- UTxO not found. 1st player revealed and won
                Just (oref', o', _) -> do   -- UTxO FOUND -->
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                    -- Must spend the UTxO that we found after the deadline has passed
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   -- Must hand back the NFT to the first player
                                   Constraints.mustPayToPubKey (spFirst sp) token
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
