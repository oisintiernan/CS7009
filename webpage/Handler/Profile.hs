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


data RepoData = RepoData{
	repo_name :: Text,
	repo_owner :: Text
}deriving(ToJSON, FromJSON, Generic, Eq, Show)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
    	sess <- getSession
    	let access_token = lookup "access_token" sess
    	let uname = lookup "login" sess
        setTitle . toHtml $ Data.Text.Encoding.decodeUtf8 (fromJust uname) <> "'s User page"
        $(widgetFile "profile")
