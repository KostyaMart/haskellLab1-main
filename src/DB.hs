{-# LANGUAGE OverloadedStrings #-}
module DB
  ( DbPool
  , withPool
  , withConn
  ) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Database.MySQL.Simple
import Data.Pool
import System.Environment (lookupEnv)
import Data.Int (Int64)
import Data.Time (NominalDiffTime)

type DbPool = Pool Connection

mkConnectInfo :: IO ConnectInfo
mkConnectInfo = do
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "DB_HOST"
  mPort <- lookupEnv "DB_PORT"
  let port :: Int
      port = maybe 3306 read mPort
  dbn  <- fromMaybe "infres"   <$> lookupEnv "DB_NAME"
  usr  <- fromMaybe "infres"   <$> lookupEnv "DB_USER"
  pwd  <- fromMaybe "infres"   <$> lookupEnv "DB_PASS"
  pure defaultConnectInfo
    { connectHost     = host
    , connectPort     = fromIntegral port
    , connectUser     = usr
    , connectPassword = pwd
    , connectDatabase = dbn
    }

withPool :: (DbPool -> IO a) -> IO a
withPool action = do
  ci <- mkConnectInfo
  let mk = connect ci
      rm = close
      stripes = 1
      idleSeconds :: NominalDiffTime
      idleSeconds = 10
      maxResPerStripe = 10
  bracket (createPool mk rm stripes idleSeconds maxResPerStripe)
          destroyAllResources
          action

withConn :: DbPool -> (Connection -> IO a) -> IO a
withConn = withResource
