{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
import           Control.Arrow          ( second )
import           Control.Monad.IO.Class ( liftIO )
import           Data.ByteString        ( ByteString )
import           Data.Maybe
import           Database.MongoDB hiding ( auth )
import           Text.Read              ( readEither )
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.MongoDB
import           Snap.Util.FileServe
import qualified Heist.Interpreted      as I
import qualified Data.ByteString.Char8  as C
import qualified Data.Text              as T
import qualified Data.Configurator      as Cfg
import           Application
import           Github.Handlers


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
         , ("",          serveDirectory "static")
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
    d <- nestMongoDBSnaplet
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d


nestMongoDBSnaplet :: Initializer b App (Snaplet MongoDB)
nestMongoDBSnaplet = do
    cfg          <- getSnapletUserConfig
    mConnections <- liftIO $ Cfg.lookup cfg "mdb_connections"
    mHost        <- liftIO $ Cfg.lookup cfg "mdb_host"
    mName        <- liftIO $ Cfg.lookup cfg "mdb_name"
    let c = fromMaybe 10 mConnections
        h = fromMaybe "127.0.0.1" mHost
        n = fromMaybe "elxa_devel" mName
    liftIO $ putStrLn $ concat [ "Opening pool of "
                               , show c
                               , " connections on "
                               , h
                               , " using db "
                               , T.unpack n
                               ]
    nestSnaplet "db" db $ mongoDBInit c (host h) n


