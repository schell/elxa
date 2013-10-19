{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty.Bounty where


import Database.MongoDB
import Data.Typeable
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Data.List        ( elemIndex, intercalate )
import qualified Data.Text as T


data BountyStatus = BountyAwaitingFunds
                  | BountyFunded
                  | BountyConfirmed
                  | BountyEscrow
                  | BountyPaid
                  | BountyUnknown deriving (Show, Eq, Typeable)


data Bounty = GithubBounty { _id      :: Maybe ObjectId
                           , _addy    :: Maybe T.Text
                           , _user    :: T.Text
                           , _repo    :: T.Text
                           , _issue   :: Int
                           , _status  :: BountyStatus
                           , _created :: UTCTime
                           , _updated :: UTCTime
                           , _total   :: Double
                           } deriving (Eq)

instance Show Bounty where
    show b = intercalate ", " [ show $ _id b
                              , show $ _addy b
                              , show $ _user b
                              , show $ _repo b
                              , show $ _issue b
                              , show $ _status b
                              , formatTime defaultTimeLocale rfc822DateFormat $ _created b
                              , formatTime defaultTimeLocale rfc822DateFormat $ _updated b
                              , show $ _total b
                              ]


emptyBounty :: Bounty
emptyBounty = GithubBounty { _id      = Nothing
                           , _addy    = Nothing
                           , _user    = ""
                           , _repo    = ""
                           , _issue   = 0
                           , _status  = BountyUnknown
                           , _created = zeroDay
                           , _updated = zeroDay
                           , _total   = 0
                           }


getNewBounty :: IO Bounty
getNewBounty = do
    t <- getCurrentTime
    i <- genObjectId
    return $ emptyBounty { _created = t
                         , _updated = t
                         , _status  = BountyAwaitingFunds
                         , _id      = Just i
                         }

instance Val BountyStatus where
    val   = statusValue
    cast' = valToBountyStatus


statusString :: BountyStatus -> T.Text
statusString BountyAwaitingFunds = "awaiting funds"
statusString BountyConfirmed = "confirmed"
statusString BountyFunded = "funded"
statusString BountyEscrow = "awaiting distribution approval"
statusString BountyPaid = "paid out"
statusString BountyUnknown = "unknown"


statusValue :: BountyStatus -> Value
statusValue = String . statusString


allStatuses :: [BountyStatus]
allStatuses = [BountyAwaitingFunds, BountyFunded, BountyConfirmed, BountyEscrow, BountyPaid, BountyUnknown]


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
    [ "type"    := String "github"
    , "user"    =: _user b
    , "repo"    =: _repo b
    , "issue"   =: _issue b
    , "status"  =: _status b
    , "created" =: _created b
    , "updated" =: _updated b
    , "total"   =: _total b
    ] ++ obj ++ addy
        where obj = case _id b of
                        Nothing  -> []
                        Just oid -> ["_id" =: oid]
              addy = case _addy b of
                         Nothing -> ["addy" := Null]
                         Just a  -> ["addy" =: a]


docToBounty :: Document -> Maybe Bounty
docToBounty d = do
    o  <- look "_id" d
    a  <- look "addy" d
    t  <- look "type" d
    t' <- cast' t
    u  <- look "user" d
    u' <- cast' u
    r  <- look "repo" d
    r' <- cast' r
    i  <- look "issue" d
    i' <- cast' i
    c  <- look "created" d
    c' <- cast' c
    q  <- look "updated" d
    q' <- cast' q
    s  <- look "status" d
    s' <- cast' s
    m  <- look "total" d
    m' <- cast' m
    b  <- makeBounty t' u' r' i' s'
    return $ b { _id   = cast' o
               , _addy = cast' a :: Maybe T.Text
               , _created = c'
               , _updated = q'
               , _total   = m'
               }


makeBounty :: T.Text -> T.Text -> T.Text -> Int -> BountyStatus -> Maybe Bounty
makeBounty "github" u r i s = Just $ GithubBounty Nothing Nothing u r i s t t 0
    where t = zeroDay
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


