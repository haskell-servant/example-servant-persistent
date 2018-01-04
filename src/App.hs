{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module App (run, User(..), mkApp, api) where

import           Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON, withObject
                            , object, (.:), (.=))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist (Key)
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , insert, entityVal)
import           Database.Persist.TH ( persistLowerCase, share, mkPersist
                                     , mkMigrate, sqlSettings)
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp

import           Servant


------------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show
|]

------------------------------------------------------------------------------
-- JSON conversion of the DB record
------------------------------------------------------------------------------

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User name age) =
    object [ "name" .= name
           , "age"  .= age  ]

------------------------------------------------------------------------------
-- Servant API
------------------------------------------------------------------------------

type Api =
       "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)

api :: Proxy Api
api = Proxy

------------------------------------------------------------------------------
-- Servant API server
------------------------------------------------------------------------------

server :: ConnectionPool -> Server Api
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. (userName newUser)] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _ -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

app :: ConnectionPool -> Application
app pool = serve api $ server pool

------------------------------------------------------------------------------
-- Run app with sqlite DB
------------------------------------------------------------------------------

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

------------------------------------------------------------------------------
-- Run sqlite app using WARP server
------------------------------------------------------------------------------

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
