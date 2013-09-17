{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Database where


import Bounty.Bounty
import Snap.Snaplet.MongoDB
import Database.MongoDB
import Control.Monad.State          ( MonadIO, MonadState )
import Control.Monad.IO.Class       ( liftIO )
import Control.Monad.Trans.Control
import Control.Applicative
import Data.Maybe
import Data.Text                    ( Text )


bountyCollection :: Text
bountyCollection = "bounties"

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

findAllAccounts :: (MonadBaseControl IO m, MonadIO m) => Action m [Document]
findAllAccounts = rest =<< find (select [] bountyCollection) { project = [ "_id" =: True ]}

getAllAccountStrings :: (MonadBaseControl IO m, MonadIO m) => Action m [String]
getAllAccountStrings = do
    docs <- findAllAccounts
    return $ catMaybes $ fmap docIdToString docs
        where docIdToString = fmap show . look "_id"


findBounty :: (MonadIO m, MonadState app m, HasMongoDB app) => String -> m (Maybe Document)
findBounty bId = do
    let obj = read bId :: ObjectId
    e   <- eitherWithDB $ findOne $ select ["_id" =: obj] bountyCollection
    return $ case e of
        Left _ -> Nothing
        Right mB -> mB


updateBountyFunding :: (Val v, MonadState app m, MonadIO m, HasMongoDB app) => String -> v -> m (Either Failure ObjectId)
updateBountyFunding bId amount = do
    let obj = read bId :: ObjectId
    e <- eitherWithDB $ modify (select ["_id" =: obj] bountyCollection) ["$set" =: ["total" =: amount]]
    return $ case e of
        Right _ -> Right obj
        Left l  -> Left l


updateBountyStatus :: (MonadState app m, MonadIO m, HasMongoDB app) => ObjectId -> m (Either Failure ())
updateBountyStatus obj = do
    e <- eitherWithDB $ findOne (select ["_id" =: obj] bountyCollection)
    case e of
        Left l           -> return $ Left l
        Right Nothing    -> return $ Left $ DocNotFound $ select ["_id" =: obj] bountyCollection
        Right (Just doc) -> do let mB = docToBounty doc
                               case mB of
                                   Nothing -> return $ Left $ QueryFailure 0 $ "Could not coerce Bounty " ++ show obj
                                   Just b  -> do b' <- liftIO $ progressStatus b
                                                 let doc' = bountyToDoc b'
                                                 eitherWithDB $ save bountyCollection doc'



