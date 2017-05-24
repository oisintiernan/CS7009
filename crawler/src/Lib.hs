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
import Data.List
import qualified Data.ByteString.Char8 as DBC
import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Data.Repos as GithubData
import qualified GitHub.Data.Name as GDN
import qualified GitHub.Data as GD 
import qualified GitHub.Endpoints.Activity.Starring as GEAS
import qualified GitHub.Endpoints.Repos.Collaborators as GERC
import qualified GitHub.Data.Definitions as GDD
import GitHub
import qualified GitHub.Auth as GA
import Database.Bolt
import qualified Data.Map as DM
import qualified Data.Maybe as DMAY
import Control.Concurrent



data Response_crawl = Response_crawl { result :: String
                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)


data UserData = UserData{ user_name :: String,
                          user_token :: String
                        } deriving(ToJSON, FromJSON, Generic, Eq, Show)







startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8020 app



app :: Application
app = serve api server

type API = "initialise_crawl" :> ReqBody '[JSON] UserData :> Post '[JSON] Response_crawl

api :: Proxy API
api = Proxy




server :: Server API
server = initialise_crawl
  where
    initialise_crawl :: UserData -> Handler Response_crawl
    initialise_crawl (UserData uname authT) = liftIO $ do
      --putStrLn "i got here"
      res <- lookupUser (Data.Text.pack uname)
      case res of
        False -> do
          liftIO $ forkIO $ get_repo (UserData uname authT)
          return $ Response_crawl "hello"
        True -> do
          liftIO $ insertUser ( Data.Text.pack uname)
          liftIO $ forkIO $ get_repo (UserData uname authT)
          return $ Response_crawl "hello"
      
  		
      --



get_repo :: UserData -> IO()
get_repo (UserData uname authT)  = liftIO $ do
  let auth = Just $ GitHub.OAuth $ (DBC.pack authT)
  let name = (GDN.N (Data.Text.pack uname))
  user_repository <- Github.userRepos' auth (name) GithubData.RepoPublicityAll
  res <- lookupUser (Data.Text.pack uname)
  case res of
    True -> do
      case user_repository of
        (Left error) -> do 
          putStrLn $ "Error: " ++ (show error)
        (Right repos) -> do
          liftIO $ insertUser ( Data.Text.pack uname)
          mapM_ (formatRepo (UserData uname authT)) repos
    False -> do
      case user_repository of
        (Left error) -> do 
          putStrLn $ "Error: " ++ (show error)
        (Right repos) -> do
          mapM_ (formatRepo (UserData uname authT)) repos
  
      
    --liftIO $ insertSomething name



--at this point have language, user, repo
formatRepo :: UserData -> Github.Repo -> IO()
formatRepo (UserData uname authT) repo = liftIO $ do
  let auth = Just $ GitHub.OAuth $ (DBC.pack authT)
  let name = (GDN.N (Data.Text.pack uname))
  let repoNames = GDN.untagName $ GithubData.repoName repo
  liftIO $ insertRepo repoNames
  liftIO $ insertOwnershipLink repoNames (Data.Text.pack uname)
  let lang = (GithubData.repoLanguage repo)
  case lang of
    Nothing -> do
      let langu = "unknown"
      res <- lookupLang (langu)
      case res of
        False -> do
          liftIO $ insertLanguageLink repoNames langu
          star_repository <- (GEAS.stargazersFor auth (name) (GD.mkRepoName repoNames)) --
          case star_repository of
            (Left error) -> do 
              putStrLn $ "Error: there is no stars"
            (Right stars) -> do
              mapM_ (formatStaryNames (UserData uname authT)) stars
        True  -> do
          liftIO $ insertLanguage langu
          liftIO $ insertLanguageLink repoNames langu
          star_repository <- (GEAS.stargazersFor auth (name) (GD.mkRepoName repoNames)) --
          case star_repository of
            (Left error) -> do 
              putStrLn $ "Error: there is no stars"
            (Right stars) -> do
              mapM_ (formatStaryNames (UserData uname authT)) stars
    (Just lang) -> do
      let langu = getLanguage lang
      res <- lookupLang (langu)
      case res of
        False -> do
            liftIO $ insertLanguageLink repoNames langu
            star_repository <- (GEAS.stargazersFor auth (name) (GD.mkRepoName repoNames)) --
            case star_repository of
              (Left error) -> do 
                putStrLn $ "Error: there is no stars"
              (Right stars) -> do
                mapM_ (formatStaryNames (UserData uname authT)) stars
        True  -> do
          --putStrLn "langu inserted"
          liftIO $ insertLanguage langu
          liftIO $ insertLanguageLink repoNames langu
          star_repository <- (GEAS.stargazersFor auth (name) (GD.mkRepoName repoNames)) --
          case star_repository of
            (Left error) -> do 
              putStrLn $ "Error: there is no stars"
            (Right stars) -> do
              mapM_ (formatStaryNames (UserData uname authT)) stars      
    







  --let repoLanguages = getLanguage (DMAY.fromJust (GithubData.repoLanguage repo))
  --putStrLn (show repoLanguages)
  --putStrLn $ "this is a repo name " ++ (show repoNames)

        --
        --let desc = (GithubData.repoDescription repos)



formatStaryNames :: UserData -> GDD.SimpleUser -> IO()
formatStaryNames (UserData uname authT) stars  = liftIO $ do
  let auth = Just $ GitHub.OAuth $ (DBC.pack authT)
  let name = (GDN.N (Data.Text.pack uname))
  let starNames = GDN.untagName $ GDD.simpleUserLogin stars
  res <- lookupUser (starNames)
  case res of
    True -> do
      liftIO $ get_repo (UserData (Data.Text.unpack starNames) authT)
    False -> do
      return()


insertOwnershipLink :: Text -> Text -> IO()
insertOwnershipLink repoName userName = do
  pipe <- connect $ def { user = "neo4j", password = "oisin" }
  result <- Database.Bolt.run pipe $ queryP cypher params
  --putStrLn "making link"
  close pipe

  where cypher = "MATCH (r:User {username: {userName}}), (l:Repo {reponame: {repoName}}) \n CREATE UNIQUE (r)-[c:owns]->(l)" 

        params = DM.fromList [ ("userName", T userName),("repoName", T repoName) ]

insertLanguageLink :: Text -> Text -> IO()
insertLanguageLink repoName langName  = do
  pipe <- connect $ def { user = "neo4j", password = "oisin" }
  result <- Database.Bolt.run pipe $ queryP cypher params
  close pipe

  where cypher = "MATCH (r:Repo {reponame: {repoName}}), (l:Lang {langname: {langName}}) \n CREATE UNIQUE (r)-[c:isWrittenIn]->(l)" 

        params = DM.fromList [ ("repoName", T repoName),("langName", T langName) ]




insertRepo :: Text -> IO [Record]
insertRepo repoName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "oisin" }
   result <- Database.Bolt.run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   Database.Bolt.close pipe
   return result
 where cypher = "CREATE (n:Repo {reponame: {repoName}}) RETURN n"
       params = DM.fromList [("repoName", Database.Bolt.T repoName)]

insertLanguage :: Text -> IO [Record]
insertLanguage langName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "oisin" }
   result <- Database.Bolt.run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   Database.Bolt.close pipe
   return result
 where cypher = "CREATE (n:Lang {langname: {langName}}) RETURN n"
       params = DM.fromList [("langName", Database.Bolt.T langName)]

insertUser :: Text -> IO [Record]
insertUser userName = do
   pipe <- Database.Bolt.connect $ def { user = "neo4j", password = "oisin" }
   result <- Database.Bolt.run pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
   Database.Bolt.close pipe
   return result
 where cypher = "CREATE (n:User {username: {userName}}) RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName)]



lookupLang :: Text -> IO Bool
lookupLang langName = do
  let neo_conf = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "oisin" }
  neo_pipe <- Database.Bolt.connect $ neo_conf 
  records <- Database.Bolt.run neo_pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
  Database.Bolt.close neo_pipe
  let isEmpty = Data.List.null records
  return isEmpty
 where cypher = "MATCH (n:Lang { langname: {langName} })RETURN n"
       params = DM.fromList [("langName", Database.Bolt.T langName)]


lookupRepo :: Text -> IO Bool
lookupRepo repoName = do
  let neo_conf = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "oisin" }
  neo_pipe <- Database.Bolt.connect $ neo_conf 
  records <- Database.Bolt.run neo_pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
  Database.Bolt.close neo_pipe
  let isEmpty = Data.List.null records
  return isEmpty
 where cypher = "MATCH (n:Repo { reponame: {RepoName} })RETURN n"
       params = DM.fromList [("repoName", Database.Bolt.T repoName)]

lookupUser :: Text -> IO Bool
lookupUser userName = do
  let neo_conf = Database.Bolt.def { Database.Bolt.user = "neo4j", Database.Bolt.password = "oisin" }
  neo_pipe <- Database.Bolt.connect $ neo_conf 
  records <- Database.Bolt.run neo_pipe $ Database.Bolt.queryP (Data.Text.pack cypher) params
  Database.Bolt.close neo_pipe
  let isEmpty = Data.List.null records
  return isEmpty
 where cypher = "MATCH (n:User { username: {userName} })RETURN n"
       params = DM.fromList [("userName", Database.Bolt.T userName)]









--RETURN r
--Need to make an initial function which just gets one repository and then adds it to the neo4j graph.


