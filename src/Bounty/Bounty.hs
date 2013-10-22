{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts #-}
module Bounty.Bounty where


import           Bounty.Types
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.List ( elemIndex )
import qualified Data.Text as T


getBountyIdInt :: Bounty -> Integer 
getBountyIdInt = unBountyId . _bountyId


emptyBounty :: Bounty
emptyBounty = GithubBounty { _bountyId = BountyId 0  
                           , _addy    = BountyAddy ""
                           , _user    = BountyUser ""
                           , _repo    = BountyRepo ""
                           , _issue   = BountyIssue 0
                           , _status  = BountyUnknown
                           , _created = zeroDay
                           , _updated = zeroDay
                           , _total   = 0
                           }


getNewBounty :: IO Bounty
getNewBounty = do
    t <- getCurrentTime
    return $ emptyBounty { _created = t
                         , _updated = t
                         , _status  = BountyAwaitingFunds
                         }


statusString :: BountyStatus -> T.Text
statusString BountyAwaitingFunds = "awaiting funds"
statusString BountyConfirmed = "confirmed"
statusString BountyFunded = "funded"
statusString BountyEscrow = "awaiting distribution approval"
statusString BountyPaid = "paid out"
statusString BountyUnknown = "unknown"


allStatuses :: [BountyStatus]
allStatuses = [BountyAwaitingFunds, BountyFunded, BountyConfirmed, BountyEscrow, BountyPaid, BountyUnknown]


allStatusStrings :: [T.Text]
allStatusStrings = fmap statusString allStatuses


stringToBountyStatus :: T.Text -> Maybe BountyStatus
stringToBountyStatus s = fmap (allStatuses !!) $ elemIndex s allStatusStrings


nextStatus :: BountyStatus -> BountyStatus
nextStatus s = case takeWhile (/= s) $ reverse allStatuses of
                   [] -> s
                   ss -> last ss


makeBounty :: T.Text -> BountyUser -> BountyRepo -> BountyIssue -> BountyStatus -> Maybe Bounty
makeBounty "github" u r i s = Just $ emptyBounty { _user = u
                                                 , _repo = r
                                                 , _issue = i
                                                 , _status = s
                                                 }
makeBounty _ _ _ _ _ = Nothing


zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)


progressStatus :: Bounty -> IO Bounty
progressStatus b
  | _total b > 0.0001 && _status b == BountyAwaitingFunds = progress b BountyFunded
  -- TODO: Actually check number of confirmations here. 
  | _total b > 0.0001 && _status b == BountyFunded = progress b BountyConfirmed
  | otherwise = progress b $ _status b
      where progress b' s = do t <- getCurrentTime
                               return $ b' { _updated = t
                                           , _status = s 
                                           }


