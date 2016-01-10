{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DB (DatabaseM, query, query_, execute, runDB) where

import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Reader (ask, liftIO, ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader (runReaderT)
import Data.Int (Int64)

newtype DatabaseM a = DatabaseM (ReaderT PG.Connection IO a)
  deriving (Monad, MonadIO, Functor, Applicative, MonadReader PG.Connection)

runDB :: DatabaseM a -> PG.Connection -> IO a
runDB (DatabaseM reader) = runReaderT reader

query :: (PG.ToRow q, PG.FromRow r) => PG.Query -> q -> DatabaseM [r]
query q r = do
  conn <- ask
  liftIO $ PG.query conn q r

query_ :: (PG.FromRow r) => PG.Query -> DatabaseM [r]
query_ q = do
  conn <- ask
  liftIO $ PG.query_ conn q

execute :: PG.ToRow q => PG.Query -> q -> DatabaseM Int64
execute q r = do
  conn <- ask
  liftIO $ PG.execute conn q r
