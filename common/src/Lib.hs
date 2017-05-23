{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib where
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import GHC.Generics
import Data.Text






data Response_crawl = Response_crawl { result :: String
                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)


data UserData = UserData{ user_name :: String,
                          user_token :: String
                        } deriving(ToJSON, FromJSON, Generic, Eq, Show)




type API = "initialise_crawl" :> ReqBody '[JSON] UserData :> Post '[JSON] Response_crawl
