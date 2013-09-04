{-# LANGUAGE OverloadedStrings #-}
module Bounty.Renders where

import Bounty.Bounty
import Control.Arrow    ( second )
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T


-- | Renders a list of github bounties in a table.
renderBounties :: Monad m => [Bounty] -> I.Splice m
renderBounties = I.callTemplate "_bounties" . splices
    where splices is = [("bounties", I.mapSplices renderBounty is)]


-- | Renders a github bounty in a tr.
renderBounty :: Monad m => Bounty -> I.Splice m
renderBounty = I.callTemplate "_bounty" . splices
    where splices = map (second I.textSplice) . texts
          texts   b = [ ("bountyId", maybe "Nothing" (T.pack . show) $ _id b)
                      , ("bountyType", "github")
                      , ("bountyUser", _user b)
                      , ("bountyRepo", _repo b)
                      , ("bountyIssue", T.pack $ show $ _issue b)
                      , ("bountyStatus", statusString $ _status b)
                      ]


-- | Renders a github user as a link.
--renderOwner :: Monad m => GithubOwner -> I.Splice m
--renderOwner u = I.callTemplate "_userLink" $ texts u
--    where texts = map (second $ I.textSplice . T.pack) . fields
--          fields GH.GithubUser{..} = makeFields (makeAvatarUrl githubOwnerGravatarId)  githubOwnerLogin githubOwnerId
--          fields GH.GithubOrganization{..} = makeFields githubOwnerAvatarUrl githubOwnerLogin githubOwnerId
--          makeAvatarUrl i = concat [ "http://www.gravatar.com/avatar/"
--                                   , i
--                                   , "?size=20"
--                                   ]
--          makeFields a l i = [ ("githubOwnerAvatarUrl", a)
--                             , ("githubOwnerLogin", l)
--                             , ("githubOwnerId", show i)
--                             ]


