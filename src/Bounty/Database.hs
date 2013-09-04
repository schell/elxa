{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Database where


import Bounty.Bounty
import Snap.Snaplet.MongoDB
import Database.MongoDB
import Control.Monad.State
import Control.Monad.Trans.Control


newUserRepoIssueTestBounty :: Bounty -> Action IO Value
newUserRepoIssueTestBounty = insert "test_bounties" . bountyToFields


findUserRepoIssueTestBounties :: (MonadBaseControl IO m, MonadIO m) => Bounty -> Action m [Document]
findUserRepoIssueTestBounties b = rest =<< find (select fields "test_bounties")
    where  fields = [ "user"   =: _user b
                    , "repo"   =: _repo b
                    , "issue"  =: _issue b
                    ]


findBounty :: (MonadIO m, MonadState app m, HasMongoDB app) => String -> m (Maybe Document)
findBounty bId = do
    let obj = read bId :: ObjectId
    e <- eitherWithDB $ findOne $ select ["_id" =: obj] "test_bounties"
    return $ case e of
        Left _ -> Nothing
        Right mB -> mB


