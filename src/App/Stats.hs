{-# LANGUAGE OverloadedStrings #-}

module App.Stats where


import Database.MongoDB
import Data.Monoid
import Data.Maybe


data AppStats = AppStats { _pollDurations :: [Double] } deriving (Show, Eq)


instance Monoid AppStats where
    mempty      = AppStats { _pollDurations = [] }
    mappend a b = AppStats { _pollDurations = take 10 $ _pollDurations b ++ _pollDurations a }


docToAppStats :: Document -> AppStats
docToAppStats d = AppStats { _pollDurations = pds }
    where mvs  = look "pollDurations" d
          mpds = maybe Nothing cast' mvs :: Maybe [Double]
          pds  = fromMaybe [] mpds


appStatsToDoc :: AppStats -> Document
appStatsToDoc a = [ "pollDurations" =: _pollDurations a ]


