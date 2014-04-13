module Main where

import Web.Scotty
import Network.HTTP.Types.Status (notFound404)

import Model
import Database.Persist hiding (get, insert)
import qualified Database.Persist as DB

import System.Environment (getEnvironment)

import Data.Maybe (fromMaybe)
import Data.List (elemIndex, mapAccumR, transpose)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Map as M

import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    conn <- getEnvDef "MONGOLAB_URI" devMongo >>= return . parseDatabaseUrl
    port <- fmap read $ getEnvDef "PORT" "8000"
    let conf = mongoConfFrom conn
    pool <- createPoolConfig conf
    scotty port (app conf pool)
    where
        devMongo = "mongodb://test:test@localhost:27017/simple-storage"
        getEnvDef e d = getEnvironment >>= return . fromMaybe d . lookup e

app conf pool = do
    get "/" $ do
        allowAllOrigins
        text "Nothing to see here *whistles*"

    get "/:slug" $ do
        allowAllOrigins
        graphId <- fmap fromSlug $ param "slug"
        graph <- runDB $ DB.selectFirst [GraphIdent ==. graphId] []
        case graph of
            Just g  -> plainJson $ fromStrict $ graphConfig `from` g
            Nothing -> text "404 not found" >> status notFound404

    put "/:slug" $ do
        allowAllOrigins
        graphId  <- fmap fromSlug $ param "slug"
        contents <- fmap (toStrict . decodeUtf8) body
        runDB $ DB.updateWhere [GraphIdent ==. graphId] [GraphConfig =. contents]
        plainJson $ fromStrict contents

    post "/" $ do
        allowAllOrigins
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
        runDB action = liftIO $ runPool conf action pool
        from f = f . entityVal
        plainJson t = text t >> setHeader "content-type" "text/json"
        allowAllOrigins = setHeader "Access-Control-Allow-Origin" "*"

        base62   = concat . transpose $ [['a'..'z'], ['0'..'9'], ['A'..'Z']]
        toSlug   = encodeWith base62
        fromSlug = decodeFrom base62

encodeWith :: String -> Int -> String
encodeWith alph num = reverse $ go alph num where
    go a 0 = [head a]
    go a i = digit : rest where
        base = length a
        digit = a !! (i `mod` base)
        remainder = i `div` base
        rest = if remainder > 0
            then go a remainder
            else ""

decodeFrom :: String -> String -> Int
decodeFrom alph = sum . snd . mapAccumR changeBase (0 :: Int) where
    base = length alph
    changeBase idx el = (idx + 1, val el * base^idx)
    val el = fromMaybe 0 (elemIndex el alph)
