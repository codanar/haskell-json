{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Avoid lambda" #-}

module MyLib where

import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Reader
import           Data
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Char                  (toLower)
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           System.Environment         (getArgs)

{- Pure code -}

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeader "project_id" [token]
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443 defaultRequest

calculateTotalLovelaces :: Address -> Int
calculateTotalLovelaces addr =
    let
        sumListAmount :: [Amount] -> Int
        sumListAmount []       = 0
        sumListAmount (a : as) =
            if unit a /= "lovelace"
                then sumListAmount as
                else convertTextToInt (quantity a) + sumListAmount as
    in
        sumListAmount $ amount addr

calculateTotalRewards :: [Reward] -> Int
calculateTotalRewards = foldr ((+) . convertTextToInt . rewardAmount) 0

convertTextToInt :: T.Text -> Int
convertTextToInt t = read $ T.unpack t :: Int

{- Impure code -}

start :: IO ()
start = do
    config <- loadConfig
    maybeCustomRequest <- runMaybeT $ runReaderT promptMenu config
    case maybeCustomRequest of
        Nothing            -> putStrLn "See you later"
        Just customRequest -> showResult customRequest >> start

loadConfig :: IO APIConfig
loadConfig = do
    args <- getArgs
    content <- readFile (head args)
    let
        linesOfContent = lines content
        aKey   = BC.pack (head linesOfContent)
        bHost  = BC.pack (linesOfContent !! 1)
        aPath  = BC.pack (linesOfContent !! 2)
        saPath = BC.pack (linesOfContent !! 3)
    putStrLn $ BC.unpack $ "ApiKey is: "                     <>
                            aKey                             <>
                            "\nHost is: "                    <>
                            bHost                            <>
                            "\nAddressModeApiPath is: "      <>
                            aPath                            <>
                            "\nStakeAddressModeApiPath is: " <>
                            saPath
    return $ APIConfig
        {
            apiKey                 = aKey,
            blockfrostHost         = bHost,
            addressModeAPIPath     = aPath,
            stakeAdressModeAPIPath = saPath
        }

promptMenu :: ReaderT APIConfig (MaybeT IO) CustomRequest -- APIConfig -> MaybeT IO Request :: IO (Maybe Request)
promptMenu = ReaderT (\config ->
    MaybeT $ do
    putStrLn "Press 'a' for address, 's' for stake address, or press 'q' to quit: "
    s <- getLine
    case toLower (head s) of
        'a' -> runMaybeT $ runReaderT buildRequestAddressMode config
        's' -> runMaybeT $ runReaderT buildRequestStakeAddressMode config
        'q' -> return Nothing
        _   -> return Nothing)

buildRequestAddressMode :: ReaderT APIConfig (MaybeT IO) CustomRequest
buildRequestAddressMode = ReaderT (\config ->
    MaybeT $ do
    putStrLn "Please enter Simple address (addr1...): "
    addr <- getLine
    if null addr
        then return Nothing
        else
            let
                req = buildRequest
                        (apiKey config)
                        (blockfrostHost config)
                        "GET"
                        (addressModeAPIPath config <> BC.pack addr)
            in
                return $ Just $ CustomRequest req AddressMode)

buildRequestStakeAddressMode :: ReaderT APIConfig (MaybeT IO) CustomRequest
buildRequestStakeAddressMode = ReaderT (\config ->
    MaybeT $ do
    putStrLn "Please enter Stake address (stake1...): "
    sAddr <- getLine
    if null sAddr
        then return Nothing
        else
            let
                req = buildRequest
                        (apiKey config)
                        (blockfrostHost config)
                        "GET"
                        (stakeAdressModeAPIPath config <> BC.pack sAddr <> "/rewards")
            in
                return $ Just $ CustomRequest req StakeAddressMode)

showResult :: CustomRequest -> IO ()
showResult cr = do
    case requestType cr of
      AddressMode -> do
        response <- httpLBS $ request cr
        let httpStatus = getResponseStatusCode response
        if httpStatus /= 200
            then do
                putStrLn "Request not succeed"
            else
                let
                    responseBody = (eitherDecode $ getResponseBody response) :: Either String Address
                in
                    case responseBody of
                        Left  err     -> putStrLn err
                        Right address -> putStrLn "Total lovelaces is:" >>
                                         print (calculateTotalLovelaces address) >>
                                         putStrLn "The stake address is: " >>
                                         print (stake_address address)
      StakeAddressMode -> do
        response <- httpLBS $ request cr
        let httpStatus = getResponseStatusCode response
        if httpStatus /= 200
            then do
                putStrLn "Request not succeed"
            else
                let
                    responseBody = (eitherDecode $ getResponseBody response) :: Either String [Reward]
                in
                    case responseBody of
                        Left  err     -> putStrLn err
                        Right rewards -> putStrLn "Total rewards is: " >> print (calculateTotalRewards rewards)
