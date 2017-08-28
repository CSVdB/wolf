{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Wolf.API where

import Import

import Data.Aeson

import Servant.API

import Wolf.Data

type WolfAPI = PersonAPI

wolfAPI :: Proxy WolfAPI
wolfAPI = Proxy

type PersonAPI
     = GetPersonEntry :<|> PostNewPerson :<|> GetPersonByAlias :<|> PostPersonSetAlias :<|> GetPersonQuery

type GetPersonEntry
     = "person" :> "entry" :> Capture "person-uuid" PersonUuid :> Get '[ JSON] PersonEntry

type PostNewPerson
     = "person" :> "new" :> ReqBody '[ JSON] PersonEntry :> Post '[ JSON] PersonUuid

type GetPersonByAlias
     = "person" :> "by-alias" :> ReqBody '[ JSON] Text :> Get '[ JSON] PersonUuid

type PostPersonSetAlias
     = "person" :> "alias" :> ReqBody '[ JSON] SetPersonAlias :> Post '[ JSON] ()

data SetPersonAlias = SetPersonAlias
    { setPersonAliasPersonUuid :: PersonUuid
    , setPersonAliasAlias :: Text
    } deriving (Show, Eq, Generic)

instance Validity SetPersonAlias

instance FromJSON SetPersonAlias

instance ToJSON SetPersonAlias

type GetPersonQuery
     = "person" :> "by-entry-query" :> ReqBody '[ JSON] PersonQuery :> Get '[ JSON] [PersonUuid]

data PersonQuery
    = EntryValue Text
                 Text
    | AndQuery PersonQuery
               PersonQuery
    deriving (Show, Read, Eq, Generic)

instance Validity PersonQuery

instance FromJSON PersonQuery

instance ToJSON PersonQuery
