import Web.Scotty
import System.Environment (getArgs)

main :: IO ()
main = do
    port <- fmap (read . head) getArgs
    scotty port $ do
        get "/" $ html "Beam me up, Scotty!"
