{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    FlexibleContexts #-}
module Bounty where

import Application
import Github.Handlers
import Snap.Snaplet
import Snap.Snaplet.MongoDB
import Snap.Snaplet.Heist
import Database.MongoDB
import Control.Monad.State
import Data.Typeable
import Control.Monad.Trans.Control
import Data.List        ( elemIndex )
import Data.Text hiding ( length, dropWhile, any, find )


data BountyStatus = BountyAwaitingFunds
                  | BountyFunded
                  | BountyEscrow
                  | BountyPaid deriving (Show, Eq, Typeable)


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


data Bounty = GithubBounty { _user   :: Text
                           , _repo   :: Text
                           , _issue  :: Int
                           , _status :: BountyStatus
                           } deriving (Show, Eq)


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


newUserRepoIssueTestBounty :: Bounty -> Action IO Value
newUserRepoIssueTestBounty = insert "test_bounties" . bountyToFields


findUserRepoIssueTestBounties :: (MonadBaseControl IO m, MonadIO m) => Bounty -> Action m [Document]
findUserRepoIssueTestBounties b = rest =<< find (select fields "test_bounties")
    where  fields = [ "user"   =: _user b
                    , "repo"   =: _repo b
                    , "issue"  =: _issue b
                    ]


createBounty :: Bounty -> Handler App App ()
createBounty b = printStuff $ "Real! " ++  unpack u ++ unpack r ++ show i
    where (u, r, i) = (_user b, _repo b, _issue b)


findBounty :: (MonadIO m, MonadState app m, HasMongoDB app) => String -> m (Maybe Document)
findBounty bId = do
    let obj = read bId :: ObjectId
    e <- eitherWithDB $ findOne $ select ["_id" =: obj] "test_bounties"
    return $ case e of
        Left _ -> Nothing
        Right mB -> mB


