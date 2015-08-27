{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

import           Control.Lens
import           Control.Monad.Reader          (local)
import           Control.Monad.State           (get)
import           Snap.AtlassianConnect
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple

data App = App
  { _heist :: Snaplet (Heist App)
  , _db    :: Snaplet Postgres
  , _connect :: Snaplet Connect
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local $ set (db . snapletValue) s

instance HasConnect (Handler b App) where
  getConnect = with connect get

type AppHandler = Handler App App

