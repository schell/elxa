{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Bounty where


import Database.MongoDB
import Data.Typeable
import Data.List        ( elemIndex )
import Data.Text hiding ( length, dropWhile, any, find )


data BountyStatus = BountyAwaitingFunds
                  | BountyFunded
                  | BountyEscrow
                  | BountyPaid deriving (Show, Eq, Typeable)


data Bounty = GithubBounty { _user   :: Text
                           , _repo   :: Text
                           , _issue  :: Int
                           , _status :: BountyStatus
                           } deriving (Show, Eq)


instance Val BountyStatus where
    val   = statusValue
    cast' = valToBountyStatus


statusString :: BountyStatus -> Text
statusString BountyAwaitingFunds = "awaiting funds"
statusString BountyFunded = "funded"
statusString BountyEscrow = "awaiting distribution approval"
statusString BountyPaid = "paid out"


statusValue :: BountyStatus -> Value
statusValue = String . statusString


allStatuses :: [BountyStatus]
allStatuses = [BountyAwaitingFunds, BountyFunded, BountyEscrow, BountyPaid]


allStatusValues :: [Value]
allStatusValues = fmap val allStatuses


allStatusStrings :: [Text]
allStatusStrings = fmap statusString allStatuses


valToBountyStatus :: Value -> Maybe BountyStatus
valToBountyStatus = takeVal
    where vals = allStatusValues
          l = length vals
          takeVal v = case dropWhile (/= v) vals of
                          _:vs -> Just $ allStatuses !! (l - (1 + length vs))
                          _    -> Nothing


stringToBountyStatus :: Text -> Maybe BountyStatus
stringToBountyStatus s = fmap (allStatuses !!) $ elemIndex s allStatusStrings


bountyToFields :: Bounty -> [Field]
bountyToFields b@GithubBounty{} =
    [ "type"   := String "github"
    , "user"   =: _user b
    , "repo"   =: _repo b
    , "issue"  =: _issue b
    , "status" =: _status b
    ]


docToBounty :: Document -> Maybe Bounty
docToBounty d = do
    t  <- look "type" d
    t' <- cast' t
    u  <- look "user" d
    u' <- cast' u
    r  <- look "repo" d
    r' <- cast' r
    i  <- look "issue" d
    i' <- cast' i
    s  <- look "status" d
    s' <- cast' s
    toBounty t' u' r' i' s'


toBounty :: Text -> Text -> Text -> Int -> BountyStatus -> Maybe Bounty
toBounty "github " u r i s = Just $ GithubBounty u r i s
toBounty _ _ _ _ _ = Nothing

