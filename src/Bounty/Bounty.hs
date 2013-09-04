{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Bounty where


import Database.MongoDB
import Data.Typeable
import Debug.Trace
import Data.List        ( elemIndex )
import qualified Data.Text as T


data BountyStatus = BountyAwaitingFunds
                  | BountyFunded
                  | BountyEscrow
                  | BountyPaid
                  | BountyUnknown deriving (Show, Eq, Typeable)


data Bounty = GithubBounty { _id     :: Maybe ObjectId
                           , _user   :: T.Text
                           , _repo   :: T.Text
                           , _issue  :: Int
                           , _status :: BountyStatus
                           } deriving (Show, Eq)

emptyBounty :: Bounty
emptyBounty = GithubBounty { _id = Nothing
                           , _user = ""
                           , _repo = ""
                           , _issue = 0
                           , _status = BountyUnknown
                           }


instance Val BountyStatus where
    val   = statusValue
    cast' = valToBountyStatus


statusString :: BountyStatus -> T.Text
statusString BountyAwaitingFunds = "awaiting funds"
statusString BountyFunded = "funded"
statusString BountyEscrow = "awaiting distribution approval"
statusString BountyPaid = "paid out"
statusString BountyUnknown = "unknown"


statusValue :: BountyStatus -> Value
statusValue = String . statusString


allStatuses :: [BountyStatus]
allStatuses = [BountyAwaitingFunds, BountyFunded, BountyEscrow, BountyPaid, BountyUnknown]


allStatusValues :: [Value]
allStatusValues = fmap val allStatuses


allStatusStrings :: [T.Text]
allStatusStrings = fmap statusString allStatuses


valToBountyStatus :: Value -> Maybe BountyStatus
valToBountyStatus = takeVal
    where vals = allStatusValues
          l = length vals
          takeVal v = case dropWhile (/= v) vals of
                          _:vs -> Just $ allStatuses !! (l - (1 + length vs))
                          _    -> Nothing


stringToBountyStatus :: T.Text -> Maybe BountyStatus
stringToBountyStatus s = fmap (allStatuses !!) $ elemIndex s allStatusStrings


nextStatus :: BountyStatus -> BountyStatus
nextStatus s = case takeWhile (/= s) $ reverse allStatuses of
                   [] -> s
                   ss -> last ss


bountyToDoc :: Bounty -> [Field]
bountyToDoc b@GithubBounty{} =
    [ "type"   := String "github"
    , "user"   =: _user b
    , "repo"   =: _repo b
    , "issue"  =: _issue b
    , "status" =: _status b
    ] ++ obj
        where obj = case _id b of
                        Nothing  -> []
                        Just oid -> ["_id" =: oid]



docToBounty :: Document -> Maybe Bounty
docToBounty d = do
    let tr a = trace (show a) a
    o  <- look "_id" d
    t  <- look "type" d
    t' <- cast' $ tr t
    u  <- look "user" d
    u' <- cast' $ tr u
    r  <- look "repo" d
    r' <- cast' $ tr r
    i  <- look "issue" d
    i' <- cast' $ tr i
    s  <- look "status" d
    s' <- cast' $ tr s
    b  <- makeBounty t' u' r' i' s'
    return $ b { _id = cast' o }


makeBounty :: T.Text -> T.Text -> T.Text -> Int -> BountyStatus -> Maybe Bounty
makeBounty "github" u r i s = Just $ GithubBounty Nothing u r i s
makeBounty _ _ _ _ _ = Nothing


progressStatus :: Bounty -> Bounty
progressStatus b = b { _status = nextStatus $ _status b }


