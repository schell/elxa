{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveDataTypeable,
    GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell #-}
module Bounty.Types where

import           Data.Data ( Data, Typeable )
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale
import           Data.SafeCopy
import           Data.List ( intercalate )
import           Data.IxSet
import qualified Data.Text as T


newtype BountyId = BountyId { unBountyId :: Integer } deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

newtype BountyAddy = BountyAddy T.Text deriving (Show, Eq, Ord, Data, Typeable, SafeCopy)


data BountyStatus = BountyAwaitingFunds
                  | BountyFunded
                  | BountyConfirmed
                  | BountyEscrow
                  | BountyPaid
                  | BountyUnknown deriving (Show, Eq, Ord, Typeable, Data)
deriveSafeCopy 0 'base ''BountyStatus


newtype BountyUser = BountyUser T.Text deriving (Show, Eq, Ord, Typeable, Data, SafeCopy)


newtype BountyRepo = BountyRepo T.Text deriving (Show, Eq, Ord, Typeable, Data, SafeCopy)


newtype BountyIssue = BountyIssue Int deriving (Show, Eq, Ord, Typeable, Data, SafeCopy)


data Bounty = GithubBounty { _bountyId :: BountyId
                           , _addy     :: BountyAddy 
                           , _user     :: BountyUser 
                           , _repo     :: BountyRepo
                           , _issue    :: BountyIssue
                           , _status   :: BountyStatus
                           , _created  :: UTCTime
                           , _updated  :: UTCTime
                           , _total    :: Double
                           } deriving (Eq, Ord, Data, Typeable)
deriveSafeCopy 0 'base ''Bounty

instance Show Bounty where
    show b = intercalate ", " [ show $ _addy b
                              , show $ _user b
                              , show $ _repo b
                              , show $ _issue b
                              , show $ _status b
                              , formatTime defaultTimeLocale rfc822DateFormat $ _created b
                              , formatTime defaultTimeLocale rfc822DateFormat $ _updated b
                              , show $ _total b
                              ]


instance Indexable Bounty where
    empty = ixSet [ ixFun $ \b -> [_bountyId b]
                  , ixFun $ \b -> [_addy b]
                  , ixFun $ \b -> [_user b]
                  , ixFun $ \b -> [_repo b]
                  , ixFun $ \b -> [_issue b]
                  , ixFun $ \b -> [_status b]
                  ]
      

