{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Handlers where


import Bounty.Bounty
import Bounty.Database
import Github.Handlers
import Application
import Snap.Snaplet
import Snap.Snaplet.MongoDB
import Snap.Snaplet.Heist
import Data.Text hiding ( length, dropWhile, any, find )


createTestBounty :: Bounty -> Handler App App ()
createTestBounty b = do
    e <- eitherWithDB $ findUserRepoIssueTestBounties b
    case e of
        Left err -> printStuff $ show err
        Right [] -> do e' <- eitherWithDB $ newUserRepoIssueTestBounty b
                       showNewTestBounty e'
        Right bs -> printStuff $ show bs


showNewTestBounty :: (Show a, Show a1, HasHeist b) => Either a a1 -> Handler b v ()
showNewTestBounty (Left e)  = printStuff $ show e
showNewTestBounty (Right i) = printStuff $ show i


createBounty :: Bounty -> Handler App App ()
createBounty b = printStuff $ "Real! " ++  unpack u ++ unpack r ++ show i
    where (u, r, i) = (_user b, _repo b, _issue b)


