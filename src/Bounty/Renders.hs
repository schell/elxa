{-# LANGUAGE OverloadedStrings #-}
module Bounty.Renders where

import Bounty.Types
import Bounty.Bounty
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Arrow    ( second )
import qualified Heist.Interpreted      as I
import qualified Data.Text              as T


-- | Renders a list of github bounties in a table.
renderBounties :: Monad m => [Bounty] -> I.Splice m
renderBounties = I.callTemplate "_bounties" . splices
    where splices is = [("bounties", I.mapSplices renderBounty is)]


formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale rfc822DateFormat

-- | Renders a github bounty in a tr.
renderBounty :: Monad m => Bounty -> I.Splice m
renderBounty = I.callTemplate "_bounty" . splices
    where splices = map (second I.textSplice) . texts
          unUser (BountyUser u) = u
          unRepo (BountyRepo r) = r
          texts   b = [ ("bountyId", (T.pack . show . unBountyId . _bountyId) b)
                      , ("bountyType", "github")
                      , ("bountyUser", unUser $ _user b)
                      , ("bountyRepo", unRepo $ _repo b)
                      , ("bountyIssue", T.pack $ show $ _issue b)
                      , ("bountyCreated", T.pack $ formatUTC $ _created b)
                      , ("bountyUpdated", T.pack $ formatUTC $ _updated b)
                      , ("bountyAddress", T.pack $ show $ _addy b)
                      , ("bountyStatus", statusString $ _status b)
                      , ("bountyTotal", T.pack $ show $ _total b)
                      ]


