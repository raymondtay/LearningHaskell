module Queries where

import           Control.Algebra
import qualified Control.Carrier.Lift            as FL
import qualified Control.Carrier.Reader          as FR
import qualified Control.Carrier.State.Strict    as FS
import qualified Control.Carrier.Writer.Strict   as FW
import qualified Control.Effect.Exception        as Ex
import           Control.Monad                   (liftM)
import           Control.Monad.IO.Class
import           Data.Bifunctor
import qualified Data.Text                       as T
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time
import           GHC.Generics                    (Generic)



data Company = Company { coyid     :: Int,
                         name      :: T.Text,
                         age       :: Int,
                         address   :: Maybe T.Text,
                         salary    :: Maybe Double,
                         join_date :: Maybe Date } deriving (Show, Generic, FromRow, ToRow)

--
-- quickStart illustrates how to connect to a running PostgreSQL database
-- instance running locally; using a connection string
--
queryString :: Query
queryString = "select * from company where name = ?"

quickStart :: String -> IO Int
quickStart uname = do
  conn <- connectPostgreSQL "host=localhost dbname=postgres password=postgres user=postgres"
  mapM_ print =<< (query_ conn "select * from company" :: IO [Company] ) -- all records
  mapM_ print =<< (query conn queryString (Only uname) :: IO [Company] )-- one record
  [Only i] <- query_ conn "select 2 + 2" :: IO [Only Int]
  return 1

--
-- quickStart' illustrates how to connect to a running PostgreSQL database
-- instance running locally; using a connection object defined in the library.
--
quickStart' :: IO Int
quickStart' = do
  conn <- connect defaultConnectInfo { connectPassword = "postgres", connectDatabase = "postgres" }
  [Only i] <- query_ conn "select ((2 + 2)/2)"
  [Only j] <- query_ conn "select count(*) from scale_data"
  return (j+i)

--
-- queryDb' demonstrates how to read the connection object from the environment
-- using 'fused-effects' library
queryDb' :: (MonadIO m, Has (FR.Reader ConnectInfo) sig m) => m ConnectInfo
queryDb' = do
  connInfo <- FR.ask
  return connInfo

--
-- queryDb demonstrates how to execute the effect, queryDb', using the
-- connection object.
--
queryDb :: (MonadIO m, Algebra sig m) => m ConnectInfo
queryDb = FR.runReader defaultConnectInfo  queryDb'

-- Annotate the type s.t. the compiler is able to execute in the IO monad.
run1 :: IO ConnectInfo
run1 = FL.runM queryDb

--
-- getConnection is another effects where it requests the connector object from
-- the environment, uses it to connect to the postgreSQL database and returns
-- the connection to consumers, downstream.
--
getConnection :: (MonadIO m,
                  MonadFail m, Has (FW.Writer String) sig m, Has (FR.Reader ConnectInfo) sig m) => m Connection
getConnection = do
  connInfo <- FR.ask
  conn     <- liftIO $ connect connInfo
  FW.tell "[Connection] Established with Db."
  return conn

-- before ->
-- runGetConnection :: (MonadFail m, MonadIO m, Algebra sig m) => m (String, Connection)
-- runGetConnection = FR.runReader defaultConnectInfo { connectPassword = "postgres" } . FW.runWriter $ getConnection
runGetConnection :: IO (String, Connection)
runGetConnection =
  FL.runM $ FR.runReader defaultConnectInfo { connectPassword = "postgres", connectDatabase = "postgres" } . FW.runWriter $ getConnection

closeConnection :: (MonadIO m, Has (FR.Reader Connection) sig m) => m ()
closeConnection = do
  conn <- FR.ask
  liftIO (close conn)
  liftIO (putStrLn "[Connection] Db connection closed.")
  return ()

runWithLogging :: IO ([String], Int)
runWithLogging = do
  (logs ,  conn) <- runGetConnection
  (logs2, count) <- FL.runM (FR.runReader conn . FW.runWriter $ queryDatabase) :: IO (String, Int)
  putStrLn $ unlines [logs, logs2]
  FL.runM $ FR.runReader conn closeConnection
  return ([logs,logs2], count)

-- Annotated the types to help the compiler resolve
runNoLogging :: IO Int
runNoLogging = do
  (_, conn) <- FL.runM $ (FR.runReader
                          defaultConnectInfo { connectPassword = "postgres" }
                          . FW.runWriter $ getConnection) :: IO (String, Connection)
  (_, count) <- FL.runM (FR.runReader conn . FW.runWriter $ queryDatabase) :: IO (String, Int)
  FL.runM $ FR.runReader conn closeConnection
  return count

getPSQLconn :: (MonadIO m, MonadFail m, Algebra sig m) => m (String, Connection)
getPSQLconn = FR.runReader defaultConnectInfo { connectPassword = "postgres" } . FW.runWriter $ getConnection

f :: (MonadIO m, MonadFail m, Algebra sig m) => m Connection
f = liftM snd getPSQLconn

f' :: (MonadIO m, MonadFail m, Algebra sig m) => m (String,Connection)
f' = liftM (bimap id id) getPSQLconn

closePSQLconn :: (MonadIO m, MonadFail m, Algebra sig m) => (String,Connection) -> m ()
closePSQLconn = \(w,c) -> liftIO (putStrLn w) >> FR.runReader c closeConnection

closePSQLconn' :: (MonadIO m, MonadFail m, Algebra sig m) => m ()
closePSQLconn' = f >>= \c -> FR.runReader c closeConnection

--
-- Note: This function achieves almost the same effect as 'runWithLogging' with
-- a caveat (See 'runG' notes for the explanation).
--
runWithLogging'' :: (MonadIO m, MonadFail m, Algebra sig m) => m ((String, Int), m ())
runWithLogging'' = getPSQLconn >>=
      \(l,c) -> (FR.runReader c . FW.runWriter $ queryDatabase) >>=
        \(ll,r) -> return (l ++ ll, r) >>=
          \(lm,rm) -> liftM (bimap id (\cx -> FR.runReader cx closeConnection)) $ return ((lm,rm),c)

--
-- Notes: runG is another way i discovered which i can write the executing of
-- the effects; an important thing to take note here is that 'f' needs to be
-- invoked otherwise i would have a connection leak.
runG :: IO (String, Int)
runG = do
  ((a,b), g) <- FL.runM (liftM (bimap id FL.runM) runWithLogging'')
  g :: IO ()
  return (a,b)

-- the 'finally' part is never run; as like the
-- Control.Exception.bracketOnError, its best not to use this in scenarios
-- where precious resources are not guaranteed to be destroyed when errors
-- happened.
runG' :: IO (String, Int)
runG' = Ex.bracketOnError getPSQLconn closePSQLconn (\(_,c) -> FR.runReader c . FW.runWriter $ queryDatabase)

runG'' :: IO (String, (String, Int))
runG'' = Ex.bracket getPSQLconn closePSQLconn (\(w,c) -> FR.runReader c . FW.runWriter . FS.runState w $ queryDatabase')

queryDatabase :: (MonadIO m,
                  MonadFail m,
                  Has (FW.Writer String) sig m,
                  Has (FR.Reader Connection) sig m) => m Int
queryDatabase = do
  conn          <- FR.ask
  [Only result] <- liftIO $ query_ conn "select count(*) from company"
  FW.tell "[Query] completed. "
  return result


queryUser :: (MonadIO m,
              MonadFail m,
              Has (FW.Writer String) sig m,
              Has (FR.Reader Connection) sig m) => String -> m Company
queryUser uname = do
  conn     <- FR.ask
  [record] <- liftIO $ query conn "select * from company where name = ?" (Only uname)
  FW.tell "[Query] record found and returned."
  return record

--
-- The small difference is that there's a State effect to store logs
queryDatabase' :: (MonadIO m,
                   MonadFail m,
                   Has (FW.Writer String) sig m,
                   Has (FS.State String) sig m,
                   Has (FR.Reader Connection) sig m) => m Int
queryDatabase' = do
  conn          <- FR.ask
  logs          <- FS.get
  [Only result] <- liftIO $ query_ conn "select count(*) from company"
  let msg = "[Query] completed. "
  FW.tell msg
  FS.put (logs ++ msg)
  return result


-- On-demand queries, to me, means that when a connection is created for the
-- purpose of servicing a query and release immediately thereafter. Compared to
-- `queryDatabase` where the connection can be used for downstream computations,
-- on-demand queries would not support this model.
queryOnDemand :: (MonadIO m,
                  MonadFail m,
                  Has (FW.Writer [String]) sig m,
                  Has (FR.Reader ConnectInfo) sig m) => m Int
queryOnDemand = do
  connInfo      <- FR.ask
  conn          <- liftIO $ connect connInfo
  [Only result] <- liftIO $ query_ conn "select count(*) from company"
  FW.tell [("[Query] completed. ")]
  return result

-- before ->
-- runQueryOnDemand :: (MonadIO m, MonadFail m, Algebra sig m) => m (String, Int)
-- runQueryOnDemand = FR.runReader defaultConnectInfo . FW.runWriter $ queryOnDemand
runQueryOnDemand :: IO ([String], Int)
runQueryOnDemand = FL.runM (FR.runReader defaultConnectInfo { connectPassword = "postgres" } . FW.runWriter $ queryOnDemand)


runQueryUser :: String -> IO (String, Company)
runQueryUser n = Ex.catch (do
  (_, c) <- runGetConnection
  FL.runM $ FR.runReader c . FW.runWriter $ (queryUser n))
  (\(e::Ex.SomeException) -> putStrLn "Error" >> return ("crap", Company{..}))

example2 :: Algebra sig m => m (Int, ())
example2 = FR.runReader "hello" . FS.runState 0 $ do
  list <- FR.ask
  FS.put (length (list :: String))

example3 :: Algebra sig m => m Int
example3 = FR.runReader "hello" . FS.execState 0 $ do
  list <- FR.ask
  FS.put (length (list :: String))
