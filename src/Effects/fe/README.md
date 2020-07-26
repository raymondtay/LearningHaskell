
fused-effects and databases
===


I ran into the [fused-effects](http://hackage.haskell.org/package/fused-effects) by accident when i was researching for ways to write effects without jumping over too many hoops with the end goal of conducting rewrites of MTL based code i've written last year.
I decided to give `fused-effects` a go even though there were others like `freer`, `freer-simple`, `polysemy` and to be quite frank, i had no idea which one to try.

A thing that bugged me when i started using this library was the fact that i did
not seem to be able to find much tutorials on it despite having registered over
7000 downloads [here](http://hackage.haskell.org/package/fused-effects) since it
was released in 2019.

In this post, i wanted to talk about my experience in using this library from
the perspective of interacting with a database which in this context, is
postgresql.

---

What was interesting for me when i started using `fused-effects` was because
its purpose was stated quite clearly for me. The main [page](http://hackage.haskell.org/package/fused-effects) reads
```
fused-effects is an effect system for Haskell that values expressivity, efficiency, and rigor.
It provides an encoding of algebraic, higher-order effects, includes a library of the most
common effects, and generates efficient code by fusing effect handlers through computations.
It is suitable for use in hobbyist, research, and industrial contexts.
```
If you like to know more about this library, feel free to bookmark this talk
[Building Haskell Programs with Fused Effects](https://www.youtube.com/watch?v=vfDazZfxlNs) by [Patrick Thompson](https://twitter.com/importantshock) & [Rob Rix](https://twitter.com/rob_rix).


The end goal, for me, is to create a connection to the database, fire a query
and retrieve its results and finally closing the connection; this pattern is
also known as [RAII](https://en.cppreference.com/w/cpp/language/raii) i.e. __resource acquisition is initialization__ which has
its genesis in the C++ world. In pseudo code, i like it to be able to run
effects in an explicit manner like the following
```haskell
runAction :: IO a
runAction = bracket getResource releaseResource useResourceToComputeSomething
```


Establishing Connection
===

The postgresql library i'm using is the [postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) and to query the postgresql database, what i needed to do was simply to make sure the properties object i.e. `defaultConnectInfo` is overrided
with the appropriate information like the host of the postgresql server, user
id , password etc. The sample code i show here is lifted from [here](http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html)

```haskell
import Database.PostgreSQL.Simple

hello :: IO Int
hello = do
  conn <- connectPostgreSQL ""
  [Only i] <- query_ conn "select 2 + 2"
  return i
```

You must have noticed that the code works in the `IO` monad which is fine for
this library but does not quite work when crafting more abstract code which
does not rely on this monad. There are several techniques that was developed
and now adopted by many haskell developers and `fused-effects`'s approach is
the following

**Note:** If you are used to `mtl`-style of writing transformer stacks, you
will find it familiar.

As with `mtl`, `fused-effects` allowed me to stack effects into the function
but reduces the cognitive overload i normally have when i read transformer
stacks and that alone is a big plus-point, for me.

```haskell
import qualified Control.Carrier.Reader as FR

getConnection :: (MonadIO m,
                  MonadFail m,
                  Has (FW.Writer String) sig m,
                  Has (FR.Reader ConnectInfo) sig m) => m Connection
getConnection = do
  connInfo <- FR.ask
  conn     <- liftIO $ connect connInfo
  FW.tell "[Connection] Established with Db."
  return conn
```

The next thing we have to do is to test things out, i write the function
`runGetConnection` which does this using the library.

```haskell
import qualified Control.Carrier.Writer as FW

runGetConnection :: IO (String, Connection)
runGetConnection =
  FL.runM $ FR.runReader defaultConnectInfo { connectPassword = "postgres", connectDatabase = "postgres" } . FW.runWriter $ getConnection

λ > do { (logs, c) <- runGetConnection ; putStrLn logs }
[Connection] Established with the Db.

```
When i run this code, i see the log message which reads `[Connection] Established with Db.`
(btw, make sure the database server is running and your passed in connector information is __correct__.)

Releasing the Connection
===

Closing the data connection follows the previous __pattern__ and its relatively
easy. The use of `liftIO` to conduct a print-line is really a escape hatch,
where i can quick test and check whether something works or not.

```haskell
closeConnection :: (MonadIO m, Has (FR.Reader Connection) sig m) => m ()
closeConnection = do
  conn <- FR.ask
  liftIO (close conn)
  liftIO (putStrLn "[Connection] Db connection closed.")
  return ()
```

Querying A Database Table
===

Triggering a query against a database typically needs an existing connection
that is __alive__. The following is how i would trigger the query which
performs a row count of the database; basically i would retrieve the database
connection from the __environment__ and use that connection to perform the
query.

```haskell
queryPsql :: (MonadIO m,
              MonadFail m,
              Has (FW.Writer String) sig m,
              Has (FR.Reader Connection) sig m) => m Int
queryPsql = do
  conn          <- FR.ask
  [Only result] <- liftIO $ query_ conn "select count(*) from company"
  FW.tell "[Query] completed. "
  return result

-- Note that the database connection it not released yet; and the logs are
-- collected and returned.
runAction :: IO ([String], Int)
runAction = do
  (l, conn) <- FL.runM $ (FR.runReader
                          defaultConnectInfo { connectPassword = "postgres" }
                          . FW.runWriter $ getConnection) :: IO (String, Connection)
  (m, count) <- FL.runM (FR.runReader conn . FW.runWriter $ queryPsql) :: IO (String, Int)
  FL.runM $ FR.runReader conn closeConnection
  return ([l,m], count)

 λ > runWithLogging
[Connection] Established with Db.
[Query] completed.
[Connection] Db connection closed.

(["[Connection] Established with Db.","[Query] completed. "],5)
```
The most direct application of running this in the `IO` monad is indicated by
the `run` or `runM` functions in the `fused-effects` library; in this example
the latter form makes more sense.

RAII
===

When you realised that you have grasped what `fused-effects` does, the next
thing to do is to discover other areas where the function can be cleaner and of
course you will discover plenty of opportunities to apply your programming
chops to the work but for now, this will do.

In Haskell, the `Control.Exception` module provides a lot of facilities to
handle exceptions from your code and more; the library i'm using here is
[fused-effects-exceptions](http://hackage.haskell.org/package/fused-effects-exceptions) which allows me to leverage functions like `bracket`
which follows the **RAII** principle and the following is how it can be
implemented

```haskell
getPSQLconn :: (MonadIO m, MonadFail m, Algebra sig m) => m (String, Connection)
getPSQLconn = FR.runReader defaultConnectInfo { connectPassword = "postgres" } . FW.runWriter $ getConnection

closePSQLconn :: (MonadIO m, MonadFail m, Algebra sig m) => (String,Connection) -> m ()
closePSQLconn = \(w,c) -> liftIO (putStrLn w) >> FR.runReader c closeConnection

-- RAII in Haskell
runAction :: IO (String, Int)
runAction = Ex.bracket getPSQLconn closePSQLconn (\(w,c) -> FR.runReader c . FW.runWriter $ queryPsql)
```

References
===

* [PostgresQL Installation](https://www.enterprisedb.com/postgres-tutorials/installation-postgresql-mac-os)
* [Effect handlers in scope](http://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf)
* [Monad Transformers and Modular Alegraic Effects: What Binds Them Together](http://www.cs.kuleuven.be/publicaties/rapporten/cw/CW699.pdf)
* [Serving HTTP content with fused-effects](https://blog.sumtypeofway.com/posts/serving-http-content-with-fused-effects.html)
* [Fusion for Free : Efficient Algebraic Effect Handlers](https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf)

