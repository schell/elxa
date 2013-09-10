{-# LANGUAGE OverloadedStrings #-}

module Polling where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Database.MongoDB hiding     ( auth )
import           Clock
import           App.Stats
import           App.Configs
import           Bounty


poll :: TMVar Double    -- ^ Time of the last poll.
     -> AppCfg          -- ^ The app configuration.
     -> IO ()
poll tm cfg = void $ forkIO $ do
    let host' = host $ _dbHost $ _mongoCfg cfg
        db = _dbName $ _mongoCfg cfg
        duration = _appPollDuration cfg
    pipe <- runIOE $ connect host'
    void $ forever $ do
        -- Lookup all bounties and check their statuses by
        -- comparing their update times and balances.
        eAccounts <- access pipe master db getAllAccountStrings
        case eAccounts of
            Left err       -> print err
            Right accounts -> print accounts
        -- Get the last app stats.
        e <- access pipe master db $ do
            stats <- findOne (select [] "stats")
            let appStats = maybe mempty docToAppStats stats
            -- The first time around, save that doc.
            when (isNothing stats) $ Database.MongoDB.save "stats" $ appStatsToDoc appStats
            return appStats
        t' <- getTime
        t  <- atomically $ takeTMVar tm

        case e of
            Right a -> do -- Update the stats with the new time.
                          let lastDur = t' - t
                              a'  = AppStats { _pollDurations = [lastDur] }
                              a'' = a `mappend` a'
                              avg = sum (_pollDurations a'') / fromIntegral (length (_pollDurations a''))
                              doc = appStatsToDoc a''


                          -- Store the new stats.
                          _ <- access pipe master db $ modify (select [] "stats") ["$set" =: doc]
                          putStrLn $ "Average poll duration:" ++ show avg
                          -- Delay this thread so we don't overload the
                          -- cpu.
                          threadDelay $ floor (duration * (10 ** 6))

            Left err -> print err

        _ <- atomically $ putTMVar tm t'
        return ()
    close pipe



