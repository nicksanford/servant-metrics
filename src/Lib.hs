-- My approch was to use STM to 
-- atomically read & write to the 
-- HealthStatus datatype which contains
-- all the health status information
-- from the webserver.

-- Thhs is all I was able to achieve in a few hours.
-- If I had more time I would try to figure out
-- how to do this same type of instrumentation
-- at the WAI middleware level so that other
-- applications could add it as middleware
-- w/o needing to change their application code.

-- I this solution hevily on this cookbook guide from the servant documentation:
-- https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}
module Lib ( startApp) where


import Servant

import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar, swapTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically, STM)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           GHC.Generics                (Generic)
import           Network.Wai.Handler.Warp    (run)

data HealthStatus = HealthStatus { requests :: Int } deriving (Show, Generic)
instance ToJSON HealthStatus
instance FromJSON HealthStatus

type GetHealthStatus = Get '[JSON] HealthStatus 
type HealthStatusAPI = "health" :> GetHealthStatus
data State = State { healthStatus :: TVar HealthStatus }

type AppM = ReaderT State Handler
server :: AppM HealthStatus
server = getHealthStatus
  where getHealthStatus :: AppM HealthStatus
        getHealthStatus = do
          State{healthStatus = p} <- ask
          liftIO $ atomically $ do
            x <- readTVar p
            swapTVar p $ increment x
            where increment :: HealthStatus -> HealthStatus
                  increment x = x{ requests = (requests x) + 1}

api :: Proxy HealthStatusAPI
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

startApp = do
  let port = 8080
  initialHealthStatus <- atomically $ newTVar $ HealthStatus 0
  run port $ app $ State initialHealthStatus