module Persistence.PostgreSQL
  ( withConnection
  ) where

import           Control.Monad.IO.Class
import qualified Data.Pool                     as P
import           Database.PostgreSQL.Simple
import           Snap.Snaplet.PostgresqlSimple (HasPostgres, Postgres (..),
                                                getPostgresState)

withConnection :: HasPostgres m => (Connection -> IO a) -> m a
withConnection f = do
  postgres <- getPostgresState
  let postgresPool = case postgres of PostgresPool pgPool -> pgPool
                                      _ -> error "Postgres connection is not a Pool"
  P.withResource postgresPool $ \conn ->
    liftIO $ withTransaction conn (f conn)
