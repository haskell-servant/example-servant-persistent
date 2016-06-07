{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text

import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show
|]

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "name"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User userName userAge) =
    object [ "name" .= userName
           , "age"  .= userAge  ]
