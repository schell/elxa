{-# LANGUAGE OverloadedStrings #-}
module Bounty.Handlers where


import           Application
import           Bounty.Types
import           Bounty.Bounty
import           Bounty.Database
import           Bounty.Renders
import           Github.Handlers
import           HandlerUtils
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.AcidState
import           Data.Maybe
import           Data.List ( nub )
import           Data.Vector ( toList )
import           Control.Monad.State
import           App.Types
import           Data.Time.Clock
import qualified Network.Bitcoin       as BTC
import qualified Heist.Interpreted     as I
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B


printBounties :: HasHeist b => [Bounty] -> Handler b v ()
printBounties bs = heistLocal (I.bindSplices $ splices bs) $ render "content"
    where splices bs' = [ ("contentTitle", I.textSplice "Bounty")
                        , ("contentBody", renderBounties bs')
                        ]


handleGithubBounty :: Handler App App ()
handleGithubBounty = method GET $ do
    eParams <- getIssueParams
    case eParams of
        Left _          -> printStuff msg
        Right (u, r, i) -> do let u' = BountyUser $ T.pack u
                                  r' = BountyRepo $ T.pack r
                                  i' = BountyIssue i
                              mBounty <- query $ BountyByUserRepoIssue u' r' i'
                              b' <- maybe 
                                      (do t   <- liftIO getCurrentTime 
                                          b   <- update $ NewBounty t
                                          app <- get
                                          a   <- liftIO $ BTC.getNewAddress (_btcAuth $ _btcCfg $ _cfg app) $ Just $ (T.pack . show . getBountyIdInt) b
                                          let b' = b { _user  = u'
                                                     , _repo  = r'
                                                     , _issue = i'
                                                     , _addy  = BountyAddy a
                                                     , _status = BountyAwaitingFunds
                                                     }
                                          void $ update $ UpdateBounty b'
                                          return b')
                                      return
                                      mBounty 
                              redirect $ B.pack ("/bounty/" ++ show (getBountyIdInt b'))
  where msg = "To open a github bounty you need a user, repo and issue number."


handleBountyStatus :: Handler App App ()
handleBountyStatus = method  GET $ do
    mBid <- getIntegerParam "bounty"
    case mBid of
        Nothing  -> printStuff "Could not parse bounty."
        Just bId -> do mB <- query $ BountyById $ BountyId bId
                       case mB of
                           Nothing -> handleHttpErr 404
                           Just b  -> printBounties [b]


handleAllBounties :: Handler App App ()
handleAllBounties = method GET $ do
    bounties <- query AllBounties
    printBounties bounties


handleProgressTestBountyStatus :: Handler App App ()
handleProgressTestBountyStatus = do
    isTesting <- fmap (_appTesting . _cfg) get
    if isTesting then progressTestBounty else handleHttpErr 404


progressTestBounty :: Handler App App ()
progressTestBounty = do
    True    <- fmap (_appTesting . _cfg) get
    mBid    <- getIntegerParam "bounty"
    when (isJust mBid) $ do
        mBounty <- query $ BountyById $ BountyId $ fromJust mBid
        if isNothing mBounty
          then handleHttpErr 404
          else let bty  = fromMaybe emptyBounty mBounty
               in do bty' <- liftIO $ progressStatus bty
                     void $ update $ UpdateBounty bty' 
                     printBounties [bty']


handleTXUpdate :: Handler App App ()
handleTXUpdate = do
    mTXID <- getStringParam "txid"
    when (isJust mTXID) $ do
        app <- get
        let btcfg = _btcCfg $ _cfg app
            btcA  = _btcAuth btcfg
            confs = _btcNumConf btcfg
        -- Get the output info from this tx.
        o <- liftIO $ BTC.getOutputInfo btcA (T.pack $ fromJust mTXID) 0
        -- Extract a list of addresses. [Address]
        let addresses = toList $ BTC.sspkAddresses $ BTC.oiScriptPubKey o
        -- Get a list of _unique_ accounts. [Account]
        accounts <- liftIO $ fmap nub $ mapM (BTC.getAccount btcA) addresses
        -- Get the amounts associated with those accounts.
        amounts  <- liftIO $ mapM (flip (BTC.getReceivedByAccount' btcA) confs) accounts
        -- Convert some things into the appropriate types.
        let amounts' = fmap (fromRational . toRational) amounts :: [Double]
            -- Our bounty ids.
            bIds     = fmap BountyId $ catMaybes $ fmap (maybeRead . T.unpack) accounts
        -- Zip our bounty ids and our amounts using the update.
        es  <- zipWithM (\bId amnt -> update $ UpdateBountyFunding bId amnt) bIds amounts'
        -- Update their status as well.
        es' <- mapM updateBountyStatus' es
        printStuff $ show es'
  where maybeRead = fmap fst . listToMaybe . reads 
        updateBountyStatus' (Just b) = do
            bounty <- liftIO $ progressStatus b 
            update $ UpdateBounty bounty
        updateBountyStatus' Nothing = return () 



