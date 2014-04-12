{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Data.Text (Text, pack, unpack, breakOn)
import qualified Data.Text as T
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Database.Persist.MongoDB
import Network (PortID (PortNumber))
import Network.URI

share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) {
    mpsGeneric = False
 }] [persistLowerCase|
Graph
    ident Int
    config Text
    deriving Show
|]

type DatabaseConnInfo = [(Text, Text)]

parseDatabaseUrl :: String -> DatabaseConnInfo
parseDatabaseUrl durl =
    let muri = parseAbsoluteURI durl
        (auth, dbpath) = case muri of
            Nothing ->  error "couldn't parse absolute uri"
            Just uri -> if uriScheme uri /= "mongodb:"
                then schemeError uri
                else case uriAuthority uri of
                    Nothing -> invalid
                    Just a  -> (a, uriPath uri)
        (user,password) = userAndPassword auth
     in
        -- Prelude.tail is unsafe but these settings should always exist.
         [(pack "user",     user)
         ,(pack "password", T.tail password)
         ,(pack "host",     pack $ uriRegName auth)
         ,(pack "port",     pack $ tail $ uriPort auth)
         ,(pack "dbname",   pack $ tail $ dbpath)
         ]
  where
    -- Prelude.init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . init . uriUserInfo
    schemeError uri = error $ "was expecting a mongodb scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    invalid = error "could not parse heroku MONGOLAB_URI"

mongoConfFrom :: DatabaseConnInfo -> MongoConf
mongoConfFrom params = MongoConf {
    mgDatabase = getParam "dbname",
    mgHost = getParam "host",
    mgPort = PortNumber $ fromIntegral . readInt . unpack $ getParam "port",
    mgAuth = Just $ MongoAuth (getParam "user") (getParam "password"),
    mgAccessMode = master,
    mgPoolStripes = 10,
    mgStripeConnections = 1,
    mgConnectionIdleTime = 1
 } where getParam n = case lookup n params of
            Just v -> v
            Nothing -> error $
                "Could not find parameter "
                ++ (show . unpack) n
                ++ " in database config URL."
         readInt = read :: String -> Int

withMongoDBConf :: (MonadBaseControl IO m, MonadIO m) => MongoConf -> Action m b -> m b
withMongoDBConf c = withMongoDBPool
                        (mgDatabase c) (unpack $ mgHost c) (mgPort c)
                        (mgAuth c) (mgPoolStripes c) (mgStripeConnections c)
                        (mgConnectionIdleTime c)
                      . runMongoDBPool master
