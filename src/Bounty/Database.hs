{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, 
    OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, 
    RecordWildCards, TypeFamilies #-}

module Bounty.Database where


import Bounty.Types
import Bounty.Bounty
import Data.Time.Clock 
import Data.IxSet
import Snap.Snaplet.AcidState
import Data.Maybe
import Data.SafeCopy
import Data.Data                    ( Data, Typeable )
import Control.Monad.Reader         ( ask )
import Control.Monad.State          ( get, put, MonadState )


-- | Acid | --


data Bounties = Bounties { _nextBountyId :: BountyId
                         , _bounties     :: IxSet Bounty
                         } deriving (Data, Typeable)
deriveSafeCopy 0 'base ''Bounties


-- | The initial state of our bounties.
initialBounties :: Bounties 
initialBounties = Bounties { _nextBountyId = BountyId 1
                           , _bounties = Data.IxSet.empty
                           }


-- | Create a new, empty bounty and add it to the db.
newBounty :: UTCTime -> Update Bounties Bounty
newBounty t = do
    bs@Bounties{..} <- get
    let bounty = emptyBounty { _bountyId = _nextBountyId 
                             , _created = t
                             , _updated = t
                             }
    -- Save the state and increment the nextBountyId.
    put $ bs { _nextBountyId = succ _nextBountyId
             , _bounties     = insert bounty _bounties
             }
    return bounty


-- | Updates a bounty in the db (indexed by BountyId).
updateBounty :: Bounty -> Update Bounties ()
updateBounty bounty = do
    b@Bounties{..} <- get
    put $ b { _bounties = updateIx (_bountyId bounty) bounty _bounties }


-- | Query a bounty indexed by BountyId.
bountyById :: BountyId -> Query Bounties (Maybe Bounty)
bountyById bid = do 
    Bounties{..} <- ask
    return $ getOne $ _bounties @= bid


-- | Query for all bounties.
allBounties :: Query Bounties [Bounty]
allBounties = do
    Bounties{..} <- ask
    return $ toList _bounties


-- | Query a bounty indexed by user, repo and issue number.
bountyByUserRepoIssue :: BountyUser -> BountyRepo -> BountyIssue -> Query Bounties (Maybe Bounty)
bountyByUserRepoIssue u r i = do
    Bounties{..} <- ask
    return $ getOne $ ((@= i) . (@= r) . (@= u)) _bounties 

-- | Updates the amount of a bounty by id or does nothing if that bounty
-- does not exist.
updateBountyFunding :: BountyId -> Double -> Update Bounties (Maybe Bounty) 
updateBountyFunding bId amount = do
    mBounty <- liftQuery $ bountyById bId
    if isJust mBounty
      then do let b = (fromJust mBounty) { _total = amount }
              updateBounty b
              return $ Just b
      else return Nothing


$(makeAcidic ''Bounties ['newBounty, 'bountyById, 'bountyByUserRepoIssue, 'updateBounty, 'updateBountyFunding, 'allBounties]) 


