module DB (DatabaseM, query, execute) where

import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Reader (ask, liftIO, ReaderT)
import Data.Int (Int64)

type DatabaseM a = ReaderT PG.Connection IO a

query :: (PG.ToRow q, PG.FromRow r) => PG.Query -> q -> DatabaseM [r]
query q r = do
  conn <- ask
  liftIO $ PG.query conn q r

execute :: PG.ToRow q => PG.Query -> q -> DatabaseM Int64
execute q r = do
  conn <- ask
  liftIO $ PG.execute conn q r
