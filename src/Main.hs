{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           Data.Text

import           Api
import           Models

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB pool query = liftIO $ runSqlPool query pool


server :: ConnectionPool -> Server TestAPI
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO ()
    userAdd newUser = flip runSqlPersistMPool pool $ do
      _ <- insert newUser
      return ()

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

testAPI :: Proxy TestAPI
testAPI = Proxy

app :: ConnectionPool -> Application
app pool = serve testAPI $ server pool

main :: IO ()
main = do
  pool <- runStderrLoggingT $ do
    createSqlitePool ":memory:" 5

  runSqlPool (runMigration migrateAll) pool
  run 8081 $ app pool
