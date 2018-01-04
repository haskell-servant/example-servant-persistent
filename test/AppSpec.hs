{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import           Api
import           App

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except

import           Data.Text

import           Models

import           Network.HTTP.Client
import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.Client

import           Test.Hspec
import           Test.Mockery.Directory

userAdd :: User -> ClientM (Maybe (Key User))
userGet :: Text -> ClientM (Maybe User)
userAdd :<|> userGet = client api

spec :: Spec
spec = do
  around withApp $ do
    describe "/user GET" $ do
      it "returns Nothing for non-existing users" $ \ port -> do
        try port (userGet "foo") `shouldReturn` Nothing

    describe "/user POST" $ do
      it "allows to add a user" $ \ port -> do
        let user = User "Alice" 1
        id <- try port (userAdd user)
        try port (userGet "Alice") `shouldReturn` Just user

      it "allows to add two users" $ \ port -> do
        let a = User "Alice" 1
        let b = User "Bob" 2
        id <- try port (userAdd a)
        id <- try port (userAdd b)
        try port (userGet "Bob") `shouldReturn` Just b

      it "returns Nothing when adding the same user twice" $ \ port -> do
        let a = User "Alice" 1
        id <- try port (userAdd a)
        try port (userAdd a) `shouldReturn` Nothing

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

try :: Int -> ClientM a -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a
