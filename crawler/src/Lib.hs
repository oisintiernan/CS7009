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
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( startApp
    , app
    ) where

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
import qualified Data.ByteString.Char8 as DBC
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Data.Repos as GithubData


data Response_crawl = Response_crawl { result :: String
                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)



type API = "initialise_crawl" :> Get '[JSON] Response_crawl


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = initialise_crawl
  where
  	initialise_crawl :: Handler Response_crawl
  	initialise_crawl = do
  		liftIO $ get_repo "mike"
  		return $ Response_crawl "hello"
  		



get_repo :: Text -> IO()
get_repo name = liftIO $ do
	user_repository <- Github.userRepos "GaryGunn94" GithubData.RepoPublicityAll
  	case user_repository of
  		(Left error) -> do 
  			putStrLn $ "Error: " ++ (show error)
  		(Right repos) -> do
  			--let desc = (GithubData.repoDescription repos)
  			mapM_ (formatRepo) repos
  			putStrLn $ "working"

formatRepo :: Github.Repo -> IO()
formatRepo repo = do
	let desc = GithubData.repoDescription repo
	putStrLn (show desc)


--Need to make an initial function which just gets one repository and then adds it to the neo4j graph.


