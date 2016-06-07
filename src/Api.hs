{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Text

import Servant.API

import Models


type TestAPI =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] ()
  :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
