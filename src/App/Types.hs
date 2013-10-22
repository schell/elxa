module App.Types where


import Network.Bitcoin
--
-- | App Configuration | --


data AppCfg = AppCfg { _appTesting      :: Bool
                     , _appPollDuration :: Double
                     , _btcCfg          :: BTCCfg
                     } deriving (Eq, Show)


data BTCCfg = BTCCfg { _btcAuth    :: Auth
                     , _btcNumConf :: Int
                     } deriving (Eq, Show)

