{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple

data APIConfig = APIConfig
    {
        apiKey                 :: BC.ByteString,
        blockfrostHost         :: BC.ByteString,
        addressModeAPIPath     :: BC.ByteString,
        stakeAdressModeAPIPath :: BC.ByteString
    }

data CustomRequest = CustomRequest
    {
        request     :: Request,
        requestType :: RequestType
    }

data RequestType = AddressMode | StakeAddressMode

{-
{
  "address": "addr1qxqs59lphg8g6qndelq8xwqn60ag3aeyfcp33c2kdp46a09re5df3pzwwmyq946axfcejy5n4x0y99wqpgtp2gd0k09qsgy6pz",
  "amount": [
    {
      "unit": "lovelace",
      "quantity": "42000000"
    },
    {
      "unit": "b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e",
      "quantity": "12"
    }
  ],
  "stake_address": "stake1ux3g2c9dx2nhhehyrezyxpkstartcqmu9hk63qgfkccw5rqttygt7",
  "type": "shelley",
  "script": false
}
-}

data Address = Address
    {
        address       :: T.Text
      , amount        :: [Amount]
      , stake_address :: T.Text
--    , type          :: T.Text
      , script        :: Bool
    } deriving (Show, Generic)

instance FromJSON Address
instance ToJSON Address

data Amount = Amount
    {
        unit     :: T.Text
      , quantity :: T.Text
    } deriving (Show, Generic)

instance FromJSON Amount
instance ToJSON Amount

{-
[
  {
  "epoch": 215,
  "amount": "12695385",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "type": "member"
  },
  {
  "epoch": 216,
  "amount": "3586329",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "type": "member"
  },
  {
  "epoch": 217,
  "amount": "1",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "type": "member"
  },
  {
  "epoch": 217,
  "amount": "1337",
  "pool_id": "pool1cytwr0n7eas6du2h2xshl8ypa1yqr18f0erlhhjcuczysiunjcs",
  "type": "leader"
  },
  {
  "epoch": 218,
  "amount": "1395265",
  "pool_id": "pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy",
  "type": "member"
  },
  {
  "epoch": 218,
  "amount": "500000000",
  "pool_id": "pool1cytwr0n7eas6du2h2xshl8ypa1yqr18f0erlhhjcuczysiunjcs",
  "type": "pool_deposit_refund"
  }
]
-}

data Reward = Reward
    {
        epoch        :: Int,
        rewardAmount :: T.Text,
        pool_id      :: T.Text
    } deriving Show

instance FromJSON Reward where
  parseJSON (Object v) =
    Reward <$> v .: "epoch"
           <*> v .: "amount"
           <*> v .: "pool_id"
