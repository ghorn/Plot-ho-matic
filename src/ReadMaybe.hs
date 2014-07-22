{-# OPTIONS_GHC -Wall #-}

module ReadMaybe
       ( readMaybe
       ) where

-- | GHC 7.4 compatability
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing
