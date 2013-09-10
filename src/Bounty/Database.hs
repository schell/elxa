{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Database where


import Bounty.Bounty
import Snap.Snaplet.MongoDB
import Database.MongoDB
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Applicative
import Data.Text ( Text )


bountyCollection :: Text
bountyCollection = "test_bounties"

newBounty :: (Applicative m, MonadIO m) => Bounty -> Action m Bounty
newBounty b = do
    oid <- insert bountyCollection $ bountyToDoc b
    return b { _id = cast' oid }


findBounties :: (MonadBaseControl IO m, MonadIO m) => Bounty -> Action m [Document]
findBounties b = rest =<< find (select fields bountyCollection)
    where  fields = [ "user"   =: _user b
                    , "repo"   =: _repo b
                    , "issue"  =: _issue b
                    ]


findBounty :: (MonadIO m, MonadState app m, HasMongoDB app) => String -> m (Maybe Document)
findBounty bId = do
    let obj = read bId :: ObjectId
    e   <- eitherWithDB $ findOne $ select ["_id" =: obj] bountyCollection
    return $ case e of
        Left _ -> Nothing
        Right mB -> mB

