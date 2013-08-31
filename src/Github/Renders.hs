{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Github.Renders where

import           Control.Arrow          ( second )
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T
import qualified Github.Repos  as GH


-- | Renders a list of github issues in a table.
renderIssues :: Monad m => [GH.Issue] -> I.Splice m
renderIssues = I.callTemplate "_issues" . splices
    where splices is = [("issues", I.mapSplices renderIssue is)]


-- | Renders a github issue in a tr.
renderIssue :: Monad m => GH.Issue -> I.Splice m
renderIssue = I.callTemplate "_issue" . splices
    where splices i = sFields i ++ tFields i
          sFields i = [ ("issueUser", renderOwner $ GH.issueUser i) ]
          tFields i = map (second I.textSplice) $ texts i
          texts   i = map (second T.pack) $ fields i
          fields  i = [ ("issueTitle", GH.issueTitle i)
                      , ("issueUpdatedAt", show $ GH.fromGithubDate $ GH.issueUpdatedAt i)
                      , ("issueNumber", show $ GH.issueNumber i)
                      ]


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

