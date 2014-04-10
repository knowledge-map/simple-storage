import Web.Scotty

import Web.Heroku.MongoDB (parseDatabaseUrl)
import Database.Persist.MongoDB (MongoConf(..), master, MongoAuth(..))
import Network (PortID (PortNumber))
import Database

import System.Environment (getEnvironment)
import System.Random (randomRIO)

import Data.Maybe (fromMaybe)
import Data.List (elemIndex, mapAccumR)
import qualified Data.Map as M

import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    conn <- getEnvDef "MONGOLAB_URI" testMongo >>= return . parseDatabaseUrl
    let mongoConf = mongoConfFrom conn

    port <- getEnvDef "PORT" 8000
    scotty port $ do
        get "/" $ text "Nothing to see here *whistles*"

        get "/:slug" $ do
            graphId <- fromSlug (param "slug")
            text "{}"
            respondJson

        put "/:slug" $ do
            graphId <- fromSlug (param "slug")
            req <- jsonData
            text req
            respondJson

        post "/" $ do
            graphId <- fmap toSlug $ rand (10000, 100000)
            json $ M.fromList [("id" :: String, show graphId)]

    where
        toSlug   = encodeWithAlphabet base62
        fromSlug = fmap (decodeFromAlphabet base62)
        base62   = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

        testMongo = "mongodb://test:test@localhost:29017/test"

        respondJson = setHeader "content-type" "text/json"
        rand = liftIO . randomRIO :: (Int, Int) -> ActionM Int
        getEnvDef e d =
            getEnvironment >>= return . fromMaybe d . fmap read . lookup e

        mongoConfFrom params = MongoConf {
            mgDatabase = getParam "dbname",
            mgHost = getParam "host",
            mgPort = PortNumber $ fromIntegral 29017,
            mgAuth = Just $ MongoAuth (getParam "username") (getParam "password"),
            mgAccessMode = master,
            mgPoolStripes = 10,
            mgStripeConnections = 1,
            mgConnectionIdleTime = 1
         } where getParam n = case lookup n params of
                    Just v -> v
                    Nothing -> error $ "Could not find parameter in database config URL."

        encodeWithAlphabet a 0 = [head a]
        encodeWithAlphabet a i = rest ++ [digit] where
            base = length a
            digit = a !! (i `mod` base)
            remainder = i `div` base
            rest = if remainder > 0
                then encodeWithAlphabet a remainder
                else ""
        decodeFromAlphabet a = sum . snd . mapAccumR changeBase 0 where
            base = length a
            changeBase index element = (index+1, val element * base^index)
            val element = fromMaybe 0 (elemIndex element a)
