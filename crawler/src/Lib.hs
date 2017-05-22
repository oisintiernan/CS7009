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
import qualified GitHub.Data.Name as GDN
import qualified GitHub.Data as GD 
import qualified GitHub.Endpoints.Repos.Collaborators as GERC
import Database.Bolt
import qualified Data.Map as DM


data Response_crawl = Response_crawl { result :: String
                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)



type API = "initialise_crawl" :> Get '[JSON] Response_crawl


startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = initialise_crawl
  where
  	initialise_crawl :: Handler Response_crawl
  	initialise_crawl = do
  		liftIO $ get_repo "GaryGunn94"
  		return $ Response_crawl "hello"
  		



get_repo :: Text -> IO()
get_repo name = liftIO $ do
	user_repository <- Github.userRepos "GaryGunn94" GithubData.RepoPublicityAll
  	case user_repository of
  		(Left error) -> do 
  			putStrLn $ "Error: " ++ (show error)
  		(Right repos) -> do
  			liftIO $ insertSomething name
  			mapM_ (formatRepo "GaryGunn94") repos
  			putStrLn $ "working"

formatRepo :: Text -> Github.Repo -> IO()
formatRepo name repo = do
	let repoNames = GDN.untagName $ GithubData.repoName repo
	putStrLn $ show repoNames
	user_repository <- (GERC.collaboratorsOn (GD.mkOwnerName name) (GD.mkRepoName repoNames))
	case user_repository of
		(Left error) -> do 
			putStrLn $ "Error: there is no collaborators"
			a <- getLine
  		(Right su) -> do
  			putStrLn (show su)

  			--let desc = (GithubData.repoDescription repos)











data Reps = Reps{
        rep_name      :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)



insertSomething :: Text -> IO [Record]
insertSomething  userName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "oisin" }
   result <- Database.Bolt.run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   Database.Bolt.close pipe
   return result
 where cypher = "CREATE (n:User {name: {userName}}) RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName)]

insertRepo :: Text -> IO [Record]
insertRepo  repoName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "oisin" }
   result <- Database.Bolt.run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   Database.Bolt.close pipe
   return result
 where cypher = "CREATE (n:Repo {name: {repoName}}) RETURN n"
       params = DM.fromList [("repoName", Database.Bolt.T repoName)]













--RETURN r
--Need to make an initial function which just gets one repository and then adds it to the neo4j graph.


