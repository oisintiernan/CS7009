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
import           Servant.JS


data Response_crawl = Response_crawl { result :: String
                                     } deriving (Show,Eq,Generic,ToJSON,FromJSON)


data UserData = UserData{ user_name :: String,
                          user_token :: String
                        } deriving(ToJSON, FromJSON, Generic, Eq, Show)



data FormatData = FormatData { lang       :: [String],
                               occurences :: [Int]  
                             } deriving(Generic, FromJSON, ToJSON, Eq, Show)

data Node = Node{ id :: String,
                  group :: String
                } deriving(ToJSON, FromJSON, Generic, Eq, Show)


data Link = Link{source :: String,
                 target :: String,
                 value :: String
                }deriving(ToJSON, FromJSON, Generic, Eq, Show)



startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8020 app



app :: Application
app = serve api server

type API = "initialise_crawl"      :> ReqBody '[JSON] UserData :> Post '[JSON] Response_crawl
           :<|> "getGraph"         :> Get '[JSON] FormatData
           :<|> "getStarGraph"     :> Get '[JSON] FormatData

api :: Proxy API
api = Proxy




server :: Server API
server = initialise_crawl :<|> getGraph :<|> getStarGraph

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
      
  	
    getGraph :: Handler FormatData
    getGraph = liftIO $ do
      putStrLn $ "Getting graph"
      getData <- langGraph
      return getData

    getStarGraph :: Handler FormatData
    getStarGraph = liftIO $ do
      putStrLn $ "Getting graph"
      getData <- selfStarGraph
      return getData
   
--------------------------------------------------------------------------------------------------
----------------------CRAWLER FUNCTIONS-----------------------------------------------------------
--------------------------------------------------------------------------------------------------


          ----GET USER REPOSITORIES SO LONG AS USER IS NOT ALREADY IN THE DATABASE----
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
  


----GET REPO DETAILS AND INSERT DATA INTO NEO, ALSO FIND OUT WHO STARRED REPO----
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
      let langu = GitHub.getLanguage lang
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
    



formatStaryNames :: UserData -> GDD.SimpleUser -> IO()
formatStaryNames (UserData uname authT) stars  = liftIO $ do
  let auth = Just $ GitHub.OAuth $ (DBC.pack authT)
  let name = (GDN.N (Data.Text.pack uname))
  let starNames = GDN.untagName $ GDD.simpleUserLogin stars
  liftIO $ insertStarLink starNames (Data.Text.pack uname)
  putStrLn $ (show starNames) ++ " starred" ++ uname
  res <- lookupUser (starNames) 
  case res of
    True -> do
      liftIO $ get_repo (UserData (Data.Text.unpack starNames) authT)
    False -> do
      return()
--------------------------------------------------------------------------------------------------
----------------------OBTAINING GRAPH INFO--------------------------------------------------------
--------------------------------------------------------------------------------------------------


selfStarGraph:: IO FormatData
selfStarGraph = do
    pipe <- connect $ def { user = "neo4j", password = "oisin" }
    noderecords <- Database.Bolt.run pipe $ Database.Bolt.query cypher
    user <- (mapM Lib.getUser) noderecords
    pop  <- (mapM getOccur) noderecords
    return (FormatData user pop)

  where cypher = "MATCH (x)-[r:starred]->(x) RETURN x.username as user, COUNT(r) as occurences ORDER BY COUNT(r) DESC LIMIT 20"


langGraph:: IO FormatData
langGraph = do
    pipe <- connect $ def { user = "neo4j", password = "oisin" }
    noderecords <- Database.Bolt.run pipe $ Database.Bolt.query cypher
    lang <- (mapM Lib.getLanguage) noderecords
    pop  <- (mapM getOccur) noderecords
    return (FormatData lang pop)

  where cypher = "MATCH (n)-[r:isWrittenIn]->(x) RETURN x.langname as lang, COUNT(r) as occurences ORDER BY COUNT(r) DESC LIMIT 20"


getLanguage :: Record -> IO String       
getLanguage input = do 
   l <- input `Database.Bolt.at` "lang" >>= exact :: IO Text
   let language = show l
   return language

getOccur :: Record -> IO Int       
getOccur input = do 
   o <- input `Database.Bolt.at` "occurences" >>= exact :: IO Int
   return o


getUser :: Record -> IO String       
getUser input = do 
   l <- input `Database.Bolt.at` "user" >>= exact :: IO Text
   let language = show l
   return language

--------------------------------------------------------------------------------------------------
----------------------NEO4J RELATIONSHIP DECLARATION----------------------------------------------
--------------------------------------------------------------------------------------------------



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

insertStarLink :: Text -> Text -> IO()
insertStarLink u1 u2  = do
  pipe <- connect $ def { user = "neo4j", password = "oisin" }
  result <- Database.Bolt.run pipe $ queryP cypher params
  putStrLn $ (show u1) ++ "starred " ++ (show u2)
  close pipe

  where cypher = "MATCH (r:User {username: {starred}}), (l:User {username: {userName}}) \n CREATE (r)-[c:starred]->(l)" 

        params = DM.fromList [ ("starred", T u1),("userName", T u2) ]

--------------------------------------------------------------------------------------------------
----------------------NEO4J NODE DECLARATION------------------------------------------------------
--------------------------------------------------------------------------------------------------


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

--------------------------------------------------------------------------------------------------
----------------------NEO4J LOOKUP FUNCTIONS------------------------------------------------------
--------------------------------------------------------------------------------------------------

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


