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
import App.Configs
import qualified Network.Bitcoin       as BTC
import qualified Heist.Interpreted     as I
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B


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

bountyIdForUserRepoIssue :: T.Text -> T.Text -> Int -> Handler App App (Maybe String)
bountyIdForUserRepoIssue u r i = do
    e <- eitherWithDB $ findBounties $ emptyBounty { _user = u
                                                   , _issue = i
                                                   , _repo = r
                                                   }
    return $ case e of
        Right [doc] -> do valOid <- look "_id" doc
                          fmap show (cast' valOid :: Maybe ObjectId)
        _           -> Nothing


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
        Right (u, r, i) -> do let u' = T.pack u
                                  r' = T.pack r
                              mbId <- bountyIdForUserRepoIssue u' r' i
                              case mbId of
                                  Just bId -> redirect $ B.pack ("/bounty/" ++ bId)
                                  Nothing  -> do b   <- liftIO getNewBounty
                                                 app <- get
                                                 a   <- liftIO $ BTC.getNewAddress (_btcAuth $ _btcCfg $ _cfg app) $ fmap (T.pack . show) $ _id b
                                                 let b'      = b { _user  = u'
                                                                 , _repo  = r'
                                                                 , _issue = i
                                                                 , _addy  = Just a
                                                                 }
                                                 createBounty b'
    where msg = "To open a github bounty you need a user, repo and issue number."


handleBountyStatus :: Handler App App ()
handleBountyStatus = method  GET $ do
    mBounty <- getStringParam "bounty"
    case mBounty of
        Nothing  -> printStuff "Could not parse bounty."
        Just bId -> do mB <- findBounty bId
                       case mB of
                           Nothing -> handleHttpErr 404
                           Just b  -> printBounties [fromMaybe emptyBounty $ docToBounty b]


handleProgressTestBountyStatus :: Handler App App ()
handleProgressTestBountyStatus = do
    isTesting <- fmap (_appTesting . _cfg) get
    if isTesting then progressTestBounty else handleHttpErr 404


progressTestBounty :: Handler App App ()
progressTestBounty = do
    True <- fmap (_appTesting . _cfg) get
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


