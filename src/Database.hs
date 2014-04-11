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

module Database where

import Prelude hiding (tail)
import qualified Prelude
import Data.Text (Text, pack, unpack, tail, breakOn)
import Database.Persist.TH
import Language.Haskell.TH.Syntax

import Database.Persist.MongoDB
import Network (PortID (PortNumber))
import Network.URI

let mongoSettings =
        (mkPersistSettings (ConT ''MongoBackend)) {
            mpsGeneric = False
         }
 in share [mkPersist mongoSettings] [persistLowerCase|
Graph
    ident Int
    config Text
    deriving Show
|]

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl durl =
    let muri = parseAbsoluteURI durl
        (auth, dbpath) = case muri of
            Nothing ->  error "couldn't parse absolute uri"
            Just uri -> if uriScheme uri /= "mongodb:"
                then schemeError uri
                else case uriAuthority uri of
                    Nothing   -> invalid
                    Just a -> (a, uriPath uri)
        (user,password) = userAndPassword auth
     in
        -- Prelude.tail is unsafe but these settings should always exist.
         [(pack "user",     user)
         ,(pack "password", tail password)
         ,(pack "host",     pack $ uriRegName auth)
         ,(pack "port",     pack $ Prelude.tail $ uriPort auth)
         ,(pack "dbname",   pack $ Prelude.tail $ dbpath)
         ]
  where
    -- Prelude.init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . Prelude.init . uriUserInfo
    schemeError uri = error $ "was expecting a mongodb scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    invalid = error "could not parse heroku MONGOLAB_URI"

mongoConfFrom params = MongoConf {
    mgDatabase = getParam "dbname",
    mgHost = getParam "host",
    mgPort = PortNumber $ fromIntegral . read . unpack $ getParam "port",
    mgAuth = Just $ MongoAuth (getParam "user") (getParam "password"),
    mgAccessMode = master,
    mgPoolStripes = 10,
    mgStripeConnections = 1,
    mgConnectionIdleTime = 1
 } where getParam n = case lookup n params of
            Just v -> v
            Nothing -> error $ "Could not find parameter " ++ (show . unpack) n ++ " in database config URL. " ++ show params

withMongoDBConf c = withMongoDBPool
                        (mgDatabase c) (unpack $ mgHost c) (mgPort c)
                        (mgAuth c) (mgPoolStripes c) (mgStripeConnections c)
                        (mgConnectionIdleTime c)
                      . runMongoDBPool master
