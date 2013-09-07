{-# LANGUAGE OverloadedStrings #-}
module Bounty.Handlers where


import Application
import Bounty.Bounty
import Bounty.Database
import Bounty.Renders
import Github.Handlers
import HandlerUtils
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.MongoDB
import Snap.Snaplet.Heist
import Database.MongoDB
import Data.Maybe
import Control.Monad.State
import qualified Network.Bitcoin   as BTC
import qualified Heist.Interpreted as I
import qualified Data.Text         as T


createBounty :: (HasMongoDB v, HasHeist b) => Bounty -> Handler b v ()
createBounty b = do
    e <- eitherWithDB $ findBounties b
    case e of
        Left err -> printStuff $ show err
        Right [] -> do e' <- eitherWithDB $ newBounty b
                       either (printStuff . show) (printBounties . (:[])) e'
        Right bs -> do let bs' = mapMaybe docToBounty bs
                       liftIO $ print bs'
                       printBounties bs'


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
        Right (u, r, i) -> do b <- liftIO getNewBounty
                              app <- get
                              let [u',r'] = fmap T.pack [u,r]
                                  a       = _btcAuth app
                                  b'      = b { _user = u'
                                              , _repo = r'
                                              , _issue = i
                                              }
                              liftIO $ print a
                              createBounty b'
    where msg = "To open a github bounty you need a user, repo and issue number."


handleBountyStatus :: Handler App App ()
handleBountyStatus = method GET $ do
    mBounty <- getStringParam "bounty"
    case mBounty of
        Nothing  -> printStuff "Could not parse bounty."
        Just bId -> do mB <- findBounty bId
                       case mB of
                           Nothing -> handleHttpErr 404
                           Just b  -> printBounties [fromMaybe emptyBounty $ docToBounty b]


handleProgressTestBountyStatus :: Handler App App ()
handleProgressTestBountyStatus = do
    isTesting <- getIsTestingEnv
    if isTesting then progressTestBounty else handleHttpErr 404

progressTestBounty :: Handler App App ()
progressTestBounty = do
    True <- getIsTestingEnv
    mBId <- getStringParam "bounty"
    mDoc <- findBounty $ fromMaybe "" mBId
    if isNothing mDoc
      then handleHttpErr 404
      else let doc  = fromJust mDoc
               mB   = docToBounty doc
               bty  = fromMaybe emptyBounty mB
           in do bty' <- liftIO $ progressStatus bty
                 _    <- eitherWithDB (save "test_bounties" $ bountyToDoc bty')
                 printBounties [bty']


