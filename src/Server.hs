import Web.Scotty
import Network.HTTP.Types.Status (notFound404)

import Model
import qualified Database.Persist as DB
import Database.Persist ((==.))
import qualified Database.Persist.MongoDB as Mongo

import System.Environment (getEnvironment)

import Data.Maybe (fromMaybe)
import Data.List (elemIndex, mapAccumR, transpose)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Map as M

import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    conn <- getEnvDef "MONGOLAB_URI" testMongo >>= return . parseDatabaseUrl
    port <- getEnvDef "PORT" 8000
    scotty port (app conn)
    where
        testMongo = "mongodb://test:test@localhost:27017/simple-storage"
        getEnvDef e d =
            getEnvironment >>= return . fromMaybe d . fmap read . lookup e

app conn = do
    get "/" $ text "Nothing to see here *whistles*"

    get "/:slug" $ do
        graphId <- fromSlug (param "slug")
        graph <- runDB $ DB.selectFirst [GraphIdent ==. graphId] []
        case graph of
            Just g  -> (text $ fromStrict $ graphConfig `from` g) >> respondJson
            Nothing -> text "404 not found" >> status notFound404

    put "/:slug" $ do
        graphId <- fromSlug (param "slug")
        req <- jsonData
        text req
        respondJson

    post "/" $ do
        contents <- fmap (toStrict . decodeUtf8) body
        graphId <- runDB $ do
            existingEnt <- DB.selectFirst [] [DB.Desc GraphIdent]
            let newId = case existingEnt of
                    Nothing  -> 10000
                    Just ent -> (graphIdent `from` ent) + 1
            DB.insert $ Graph newId contents
            return newId
        json $ M.fromList [("id" :: String, show $ toSlug $ graphId)]

    where
        runDB = liftIO . withMongoDBConf (mongoConfFrom conn)
        from f = f . Mongo.entityVal
        respondJson = setHeader "content-type" "text/json"

        base62   = concat . transpose $ [['a'..'z'], ['0'..'9'], ['A'..'Z']]
        toSlug   = encodeWithAlphabet base62
        fromSlug = fmap (decodeFromAlphabet base62)

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
