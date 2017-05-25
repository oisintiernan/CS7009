{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Handler.Profile where

import Import hiding (unpack, pack)
import Data.List hiding(intercalate, map, lookup)
import qualified GitHub.Endpoints.Repos as Github
import GitHub.Data as GHD
import GitHub.Data.Repos as GHDR
import Data.Maybe
import Data.Aeson
import Data.Text.Encoding
import Data.Vector as V hiding (mapM)
import Data.Text hiding(intercalate, map, lookup)
import qualified Servant as S
import Servant.API
import Servant.Client
import qualified Servant.Server as SS
import Network.HTTP.Client
import Data.ByteString.Char8 as DBC hiding (putStrLn)
import Data.Text as DT hiding(intercalate, map) 
import Lib

--data UserData = UserData{ user_name :: String,
--                          user_token :: String
--                        } deriving(ToJSON, FromJSON, Generic, Eq, Show)

--data RepoData = RepoData{ repo_name :: Text,
--                          repo_owner :: Text
--                        } deriving(ToJSON, FromJSON, Generic, Eq, Show)

--data Response_crawl = Response_crawl { result :: String
--                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)




api :: S.Proxy Lib.API
api = S.Proxy

initialise_crawl :: UserData -> ClientM Response_crawl
getGraph :: ClientM FormatData
getStarGraph :: ClientM FormatData

(initialise_crawl :<|> getGraph :<|> getStarGraph) = client api

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
        let access_token = DBC.unpack (fromJust $ Import.lookup "access_token" sess)
        let uname = DBC.unpack (fromJust $ Import.lookup "login" sess)
    	--liftIO $ makeCall (UserData "phadej" access_token)
    	setTitle . toHtml $ (DT.pack uname) <> "'s User page"
        $(widgetFile "profile")

makeCall :: Lib.UserData -> IO()
makeCall user = liftIO $ do
	manager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
	res <- runClientM (initialise_crawl (user)) (ClientEnv manager (BaseUrl Http "localhost" (8020) ""))
	case res of
		Left err -> Import.putStrLn "gwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww"
		Right response -> return ()

