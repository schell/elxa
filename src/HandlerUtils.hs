{-# LANGUAGE OverloadedStrings #-}
module HandlerUtils where


import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Data.Maybe ( listToMaybe )
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T
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


getIntParam :: C.ByteString -> Handler App a (Maybe Int)
getIntParam p = do
    mInt <- getStringParam p
    return $ case mInt of
        Nothing -> Nothing
        Just s  -> maybeRead s
    where maybeRead = fmap fst . listToMaybe . reads


getIntegerParam :: C.ByteString -> Handler App a (Maybe Integer)
getIntegerParam p = do
    mInt <- getIntParam p
    return $ fmap fromIntegral mInt 


errors :: Int -> T.Text
errors 404 = "URL not found :("
errors _   = "Unknown error."


handleHttpErr :: HasHeist b => Int -> Handler b v ()
handleHttpErr c = do
    modifyResponse (setResponseCode c)
    heistLocal binding $ render "error"
        where binding = I.bindSplices splices
              splices = [ ("errCode", I.textSplice $ T.pack $ show c)
                        , ("errText", I.textSplice $ errors c)
                        ]


