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

module AuthNFTIssuer(issuerCS, endpoints, AuthNFTIssuerSchema) where

import           Control.Lens         (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BSU
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet (Wallet, walletPubKey)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

type AuthNFTIssuerSchema =
                  Endpoint "mint" Wallet
              .\/ Endpoint "inspect" String
              .\/ Endpoint "logWalletNftTokenName" ()

mint :: forall w s e. AsContractError e => Wallet -> Contract w s e ()
mint wallet = do
    pkh <- pubKeyHash <$> ownPubKey
    let reqPk = (pubKeyHash . walletPubKey) wallet
        val     = Value.singleton (issuerCS pkh) (TokenName $ getPubKeyHash reqPk) 1
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val <>
                  Constraints.mustPayToPubKey reqPk val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "minted %s" (show val)

inspect :: forall w s e. AsContractError e => String -> Contract w s e ()
inspect _ = do
    logInfo @String $ "Inspecting own utxos."
    pk  <- ownPubKey
    os  <- map snd . Map.toList <$> utxosAt (pubKeyAddress pk)
    let totalVal = mconcat [view ciTxOutValue o | o <- os]
    logInfo @String
            $ "Logging total Value : " <> show totalVal
    logInfo @String $ "Inspect complete"

logWalletNftTokenName :: forall w s e. AsContractError e => () -> Contract w s e ()
logWalletNftTokenName _ = do
    pkh <- pubKeyHash <$> ownPubKey
    let tn = TokenName $ getPubKeyHash pkh
    logInfo @String
            $ "Logging own nft token name : " <> show tn
    logInfo @String $ "logWalletNftTokenName complete"

mint' :: Promise () AuthNFTIssuerSchema Text ()
mint' = endpoint @"mint" mint

inspect' :: Promise () AuthNFTIssuerSchema Text ()
inspect' = endpoint @"inspect" inspect

logWalletNftTokenName' :: Promise () AuthNFTIssuerSchema Text ()
logWalletNftTokenName' = endpoint @"logWalletNftTokenName" logWalletNftTokenName

endpoints :: AsContractError e => Contract () AuthNFTIssuerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [mint',inspect',logWalletNftTokenName'] >>  endpoints

mkSchemaDefinitions ''AuthNFTIssuerSchema
mkKnownCurrencies []
