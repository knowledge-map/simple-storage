import Web.Scotty

import System.Environment (getEnvironment)
import System.Random (randomRIO)

import Data.Maybe (fromMaybe)
import Data.List (elemIndex, mapAccumR)
import qualified Data.Map as M

import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    port <- getEnvDef "PORT" 8000
    scotty port $ do
        get "/" $ text "Nothing to see here *whistles*"

        get "/:slug" $ do
            graphId <- fromSlug (param "slug")
            json $ M.fromList [("id" :: String, show graphId)]

        put "/:slug" $ do
            graphId <- fromSlug (param "slug")
            json $ M.fromList [("id" :: String, show graphId)]

        post "/" $ do
            graphId <- rand (0, 10000)
            json $ M.fromList [("id" :: String, show graphId)]

    where
        toSlug x = return $ encodeWithAlphabet base62 x
        fromSlug = fmap    (decodeFromAlphabet base62)
        base62 = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

        rand = liftIO . randomRIO :: (Int, Int) -> ActionM Int
        getEnvDef e d =
            getEnvironment >>= return . fromMaybe d . fmap read . lookup e

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
