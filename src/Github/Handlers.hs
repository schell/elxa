{-# LANGUAGE OverloadedStrings #-}
module Github.Handlers where
import           Control.Applicative
import           Control.Monad.IO.Class ( liftIO )
import           Text.Read              ( readEither )
import           Data.Maybe
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Application
import           Github.Renders
import           HandlerUtils
import qualified Github.Repos  as GH
import qualified Github.Issues as GH
import qualified Heist.Interpreted      as I
import qualified Data.ByteString.Char8  as C
import qualified Data.Text              as T


-- | Handles showing github things.
handleGithub :: Handler App App ()
handleGithub = method GET $ printStuff "blah!\nblah..."


-- | Handles showing github user info.
handleGithubUser :: Handler App App ()
handleGithubUser = method GET $ do
    mUser <- (C.unpack <$>) <$> getParam "user"
    if isNothing mUser
      then printStuff "No user specified."
      else do eRepos <- liftIO $ GH.userRepos (fromJust mUser) GH.All
              printStuff $ show eRepos


-- | Handles showing github repo info.
handleGithubUserRepo :: Handler App App ()
handleGithubUserRepo = method GET $ do
    mUser <- (C.unpack <$>) <$> getParam "user"
    mRepo <- (C.unpack <$>) <$> getParam "repo"
    if isNothing mUser || isNothing mRepo
      then printStuff "No repo specified."
      else do eRepo <- liftIO $ GH.userRepo (fromJust mUser) (fromJust mRepo)
              printStuff $ show eRepo


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


type GithubIssueParams = (String, String, Int)

-- | Handles showing github issue info.
handleGithubUserRepoIssue :: Handler App App ()
handleGithubUserRepoIssue = method GET $ do
    eParams <- getIssueParams
    case eParams of
        Left s  -> printStuff s
        Right (user, repo, issue) -> printIssue user repo issue
                         where printErr         = printStuff . show
                               tSp              = I.textSplice . T.pack
                               contentSplices i = [ ("contentTitle", tSp "Issues")
                                                  , ("contentBody", issues i)
                                                  ]
                               issues           = renderIssues . (:[])
                               printIssues i    = heistLocal (I.bindSplices $ contentSplices i) $ render "content"
                               printIssue u r i = do eI <- liftIO $ GH.issue u r i
                                                     either printErr printIssues eI


getIssueParam :: Handler App a (Either String Int)
getIssueParam = do
    mIssue <- getStringParam "issue"
    let eNum = case mIssue of
                   Just i  -> readEither i :: Either String Int
                   Nothing -> Left "No issue specified."
    return eNum


getIssueParams :: Handler App a (Either String GithubIssueParams)
getIssueParams = do
    mUser  <- getStringParam "user"
    mRepo  <- getStringParam "repo"
    eIssue <- getIssueParam
    return $ case (mUser, mRepo, eIssue) of
        (Nothing, _, _) -> Left "No user specified."
        (_, Nothing, _) -> Left "No repo specified."
        (_, _, Left s)  -> Left s
        (Just u, Just r, Right i) -> Right (u, r, i)


