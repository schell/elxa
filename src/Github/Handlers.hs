{-# LANGUAGE OverloadedStrings #-}
module Github.Handlers where
import           Control.Applicative
import           Control.Monad.IO.Class ( liftIO )
import           Data.Maybe
import           Text.Read              ( readEither )
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Application
import           Github.Renders
import qualified Github.Repos  as GH
import qualified Github.Issues as GH
import qualified Heist.Interpreted      as I
import qualified Data.ByteString.Char8  as C
import qualified Data.Text              as T


-- | Helper for printing things.
printStuff :: HasHeist b => String -> Handler b v ()
printStuff stuff = heistLocal binding $ render "print"
  where binding = I.bindSplice "stuff" tSplice
        tSplice = I.textSplice $ T.pack stuff


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



