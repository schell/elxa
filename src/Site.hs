{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Site
-- Copyright   :  Schell Scivally August 29, 2013
--
-- Maintainer  :  efsubenovex@gmail.com
-- Stability   :  experimental
-- Portability :  Portable
--
-- This is the main routing and logic for project elxa, a btc bugpool.
--

module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.IO.Class      ( liftIO )
import           Data.ByteString             ( ByteString )
import           Data.Maybe
import           Database.MongoDB hiding     ( auth )
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.MongoDB
import           Snap.Util.FileServe
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T
import qualified Data.Configurator      as Cfg
import qualified Network.Bitcoin        as BTC
import           Application
import           Bounty
import           Github.Handlers
import           Clock
import           App.Configs


handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/github", handleGithub)
         , ("/github/:user", handleGithubUser)
         , ("/github/:user/:repo", handleGithubUserRepo)
         , ("/github/:user/:repo/issues", handleGithubUserRepoIssues)
         , ("/github/:user/:repo/issue/:issue", handleGithubUserRepoIssue)
         , ("/bounty/github/:user/:repo/:issue", handleGithubBounty)
         , ("/bounty/:bounty", handleBountyStatus)
         , ("/tx/:txid", handleTXUpdate)
         , ("", serveDirectory "static")
         ]


-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "The BTC based bounty service." Nothing $ do
    h   <- nestSnaplet "" heist $ heistInit "templates"
    s   <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    appCfg@(AppCfg _ _ mCfg bCfg) <- getAppCfg

    d <- nestMongoDBSnaplet mCfg

    addRoutes routes
    addAuthSplices h auth

    btcInfo <- liftIO $ BTC.getBitcoindInfo (_btcAuth bCfg)
    liftIO $ putStrLn "Bitcoind info:\n"
    liftIO $ print btcInfo

    tm  <- liftIO $ do
        t  <- getTime
        atomically $ newTMVar t

    return $ App h s a d appCfg tm


nestMongoDBSnaplet :: MongoCfg -> Initializer b App (Snaplet MongoDB)
nestMongoDBSnaplet (MongoCfg c h n) = do
    liftIO $ putStrLn $ concat [ "Opening pool of "
                               , show c
                               , " connections on "
                               , h
                               , " using db "
                               , T.unpack n
                               ]
    nestSnaplet "db" db $ mongoDBInit c (host h) n


getAppCfg :: Initializer App App AppCfg
getAppCfg = do
    btcCfg <- getBTCCfg
    mdbCfg <- getMongoCfg
    cfg'   <- getSnapletUserConfig
    mPoll  <- liftIO $ Cfg.lookup cfg' "poll_duration"
    mTest  <- liftIO $ Cfg.lookup cfg' "testing"
    let d = fromMaybe 10 mPoll
        t = fromMaybe False mTest
    return $ AppCfg t d mdbCfg btcCfg


getBTCCfg :: Initializer App App BTCCfg
getBTCCfg = do
    cfg'  <- getSnapletUserConfig
    mUrl  <- liftIO $ Cfg.lookup cfg' "btc_url"
    mUser <- liftIO $ Cfg.lookup cfg' "btc_user"
    mPass <- liftIO $ Cfg.lookup cfg' "btc_pass"
    mConfs<- liftIO $ Cfg.lookup cfg' "btc_confs"
    let url  = fromMaybe "http://localhost:18332" mUrl
        user = fromMaybe "bitcoinrpc" mUser
        pasw = fromMaybe "bitcoinrpcpassword" mPass
        conf = maybe 0 read mConfs
        btcA = BTC.Auth url user pasw
    liftIO $ putStrLn $ concat [ "Connecting to bitcoind at "
                               , T.unpack url
                               , " using user "
                               , T.unpack user
                               ]
    return $ BTCCfg btcA conf


getMongoCfg :: Initializer App App MongoCfg
getMongoCfg = do
    cfg'          <- getSnapletUserConfig
    mConnections  <- liftIO $ Cfg.lookup cfg' "mdb_connections"
    mHost         <- liftIO $ Cfg.lookup cfg' "mdb_host"
    mName         <- liftIO $ Cfg.lookup cfg' "mdb_name"
    let c = fromMaybe 10 mConnections
        h = fromMaybe "127.0.0.1" mHost
        n = fromMaybe "elxa_devel" mName
    return $ MongoCfg c h n


