{-# LANGUAGE OverloadedStrings #-}
module HandlerUtils where


import           Control.Monad.IO.Class      ( liftIO )
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T
import qualified Data.Configurator      as Cfg
import qualified Data.ByteString.Char8  as C
import           Application


-- | Helper for printing things.
printStuff :: HasHeist b => String -> Handler b v ()
printStuff stuff = heistLocal binding $ render "print"
  where binding = I.bindSplice "stuff" tSplice
        tSplice = I.textSplice $ T.pack stuff


getStringParam :: C.ByteString -> Handler App a (Maybe String)
getStringParam p = do
    mV <- getParam p
    return $ case mV of
        Just v  -> Just $ C.unpack v
        Nothing -> Nothing


getIsTestingEnv :: Handler a b Bool
getIsTestingEnv = do
    cfg   <- getSnapletUserConfig
    mTest <- liftIO $ Cfg.lookup cfg "testing"
    liftIO $ print mTest
    case mTest of
        Just True -> return True
        _         -> return False


errors :: Int -> String
errors 404 = "URL not found :("
errors _   = "Unknown error."


handleHttpErr :: HasHeist b => Int -> Handler b v ()
handleHttpErr c = do
    modifyResponse (setResponseCode c)
    printStuff $ errors c


