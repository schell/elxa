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

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
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
import qualified Github.Repos           as GH
import qualified Github.Issues          as GH
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Helper for printing things.
printStuff :: HasHeist b => String -> Handler b v ()
printStuff stuff = heistLocal binding $ render "print"
  where binding = I.bindSplice "stuff" tSplice
        tSplice = I.textSplice $ T.pack stuff


------------------------------------------------------------------------------
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | Handles showing github things.
handleGithub :: Handler App App ()
handleGithub = method GET $ printStuff "blah!\nblah..."


------------------------------------------------------------------------------
-- | Handles showing github user info.
handleGithubUser :: Handler App App ()
handleGithubUser = method GET $ do
    mUser <- (C.unpack <$>) <$> getParam "user"
    if isNothing mUser
      then printStuff "No user specified."
      else do eRepos <- liftIO $ GH.userRepos (fromJust mUser) GH.All
              printStuff $ show eRepos


------------------------------------------------------------------------------
-- | Handles showing github repo info.
handleGithubUserRepo :: Handler App App ()
handleGithubUserRepo = method GET $ do
    mUser <- (C.unpack <$>) <$> getParam "user"
    mRepo <- (C.unpack <$>) <$> getParam "repo"
    if isNothing mUser || isNothing mRepo
      then printStuff "No repo specified."
      else do eRepo <- liftIO $ GH.userRepo (fromJust mUser) (fromJust mRepo)
              printStuff $ show eRepo


------------------------------------------------------------------------------
-- | Handles showing github repo issues info.
handleGithubUserRepoIssues :: Handler App App ()
handleGithubUserRepoIssues = method GET $ do
    mUser <- (C.unpack <$>) <$> getParam "user"
    mRepo <- (C.unpack <$>) <$> getParam "repo"
    if isNothing mUser || isNothing mRepo
      then printStuff "No repo specified."
      else do eIssues <- liftIO $ GH.issuesForRepo (fromJust mUser) (fromJust mRepo) []
              either printLeft printRight eIssues
                where printLeft     = printStuff . show
                      printRight is = heistLocal (I.bindSplices $ splices is) $ render "content"
                      splices    is = [ ("contentTitle", tSp "Issues")
                                      , ("contentBody", renderIssues is)
                                      ]
                      tSp           = I.textSplice . T.pack


------------------------------------------------------------------------------
-- | Handles showing github issue info.
handleGithubUserRepoIssue :: Handler App App ()
handleGithubUserRepoIssue = method GET $ do
    mUser  <- (C.unpack <$>) <$> getParam "user"
    mRepo  <- (C.unpack <$>) <$> getParam "repo"
    mIssue <- (C.unpack <$>) <$> getParam "issue"
    if isNothing mUser || isNothing mRepo || isNothing mIssue
      then printStuff "No issue specified."
      else do let [user, repo, issue] = fmap fromJust [mUser, mRepo, mIssue]
                  eNum                = readEither issue :: Either String Int
              either printLeft (printIssue user repo) eNum
                where printLeft        = printStuff
                      printErr         = printStuff . show
                      tSp              = I.textSplice . T.pack
                      contentSplices i = [ ("contentTitle", tSp "Issues")
                                         , ("contentBody", issues i)
                                         ]
                      issues           = renderIssues . (:[])
                      printIssues i    = heistLocal (I.bindSplices $ contentSplices i) $ render "content"
                      printIssue u r i = do eIssue <- liftIO $ GH.issue u r i
                                            either printErr printIssues eIssue


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "The BTC based bounty service." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    d <- nestSnaplet "db" db $ mongoDBInit 10 (host "127.0.0.1") "elxa"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d


------------------------------------------------------------------------------
-- RENDERING
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Renders a list of github issues in a table.
renderIssues :: Monad m => [GH.Issue] -> I.Splice m
renderIssues = I.callTemplate "_issues" . splices
    where splices is = [("issues", I.mapSplices renderIssue is)]


------------------------------------------------------------------------------
-- | Renders a github issue in a tr.
renderIssue :: Monad m => GH.Issue -> I.Splice m
renderIssue = I.callTemplate "_issue" . splices
    where splices i = sFields i ++ tFields i
          sFields i = [ ("issueUser", renderOwner $ GH.issueUser i) ]
          tFields i = map (second I.textSplice) $ texts i
          texts   i = map (second T.pack) $ fields i
          fields  i = [ ("issueTitle", GH.issueTitle i)
                      , ("issueUpdatedAt", show $ GH.fromGithubDate $ GH.issueUpdatedAt i)
                      , ("issueId", show $ GH.issueId i)
                      , ("issueNumber", show $ GH.issueNumber i)
                      ]


------------------------------------------------------------------------------
-- | Renders a github user as a link.
renderOwner :: Monad m => GH.GithubOwner -> I.Splice m
renderOwner u = I.callTemplate "_userLink" $ texts u
    where texts = map (second $ I.textSplice . T.pack) . fields
          fields GH.GithubUser{..} = makeFields (makeAvatarUrl githubOwnerGravatarId)  githubOwnerLogin githubOwnerId
          fields GH.GithubOrganization{..} = makeFields githubOwnerAvatarUrl githubOwnerLogin githubOwnerId
          makeAvatarUrl i = concat [ "http://www.gravatar.com/avatar/"
                                   , i
                                   , "?size=20"
                                   ]
          makeFields a l i = [ ("githubOwnerAvatarUrl", a)
                             , ("githubOwnerLogin", l)
                             , ("githubOwnerId", show i)
                             ]

