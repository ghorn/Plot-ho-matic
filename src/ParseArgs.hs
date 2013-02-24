{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ParseArgs ( getip
                 ) where

import System.Console.CmdArgs

data VisArgs = VisArgs { ipfile :: String
                       , ip :: String
                       } deriving (Show, Data, Typeable)

myargs :: VisArgs
myargs = VisArgs { ipfile = "" &= help "file to read IP address out of" &= typ "FILENAME"
                 , ip = ""     &= help "an IP address" &= typ "ADDRESS"
                 } &= summary "the kite visualizer program"

getip :: String -> String -> IO String
getip programname defaultip = do
  a <- cmdArgs (myargs &= program programname)
  case (ipfile a,ip a) of
    ("","") -> return defaultip
    ("",x) -> return x
    (f,"") -> fmap (head . lines) (readFile f)
    (_,_) -> error "please only specify your ip address one way"
--  
--main :: IO ()
--main = do
--  ip' <- getip "defaultip"
--  print ip'
--  putStrLn "finished successfully"
