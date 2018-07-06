{-# OPTIONS_GHC -Wall #-}
{-# Language CPP #-}

module PlotHoCommon
  ( getRTSStatsEnabled
  , getLiveBytes
  ) where

import qualified GHC.Stats

getRTSStatsEnabled :: IO Bool
#if __GLASGOW_HASKELL__ >= 802
getRTSStatsEnabled = GHC.Stats.getRTSStatsEnabled
#else
getRTSStatsEnabled = GHC.Stats.getGCStatsEnabled
#endif

getLiveBytes :: IO Double
#if __GLASGOW_HASKELL__ >= 802
getLiveBytes = realToFrac . GHC.Stats.gcdetails_live_bytes . GHC.Stats.gc <$> GHC.Stats.getRTSStats
#else
getLiveBytes = realToFrac . GHC.Stats.currentBytesUsed <$> GHC.Stats.getGCStats
#endif
