module App.Configs where


import Data.Text
import Network.Bitcoin


data AppCfg = AppCfg { _appTesting      :: Bool
                     , _appPollDuration :: Double
                     , _mongoCfg        :: MongoCfg
                     , _btcCfg          :: BTCCfg
                     } deriving (Eq, Show)


data MongoCfg = MongoCfg { _dbConnections :: Int
                         , _dbHost        :: String
                         , _dbName        :: Text
                         } deriving (Show, Eq)


data BTCCfg = BTCCfg { _btcAuth    :: Auth
                     , _btcNumConf :: Int
                     } deriving (Eq, Show)

