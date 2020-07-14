import System.Directory (Permissions(..))
import System.Time (ClockTime(..))
import System.FilePath ((</>))
import Control.Monad
import System.Directory
import System.IO
import Control.Exception

data Info = Info {
    infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)


data Iterate seed =
    Done        { unwrap :: seed }
    | Skip      { unwrap :: seed }
    | Continue  { unwrap :: seed }
    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

countDirectories count info = Continue (if isDirectory info then count + 1 else count)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3 = Done paths
    | isDirectory info && takeFileName path == ".svn" = Skip paths
    |   extension `elem` [".jpg", ".png"] = Continue (path : paths)
    | otherwise = Continue paths
        where extension = map toLower (takeExtension path)
              path = infoPath info
    
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
    where
        fold seed subpath = getUsefulContents subpath >>= walk seed

        walk seed (name : names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _) -> return done
                Skip seed'    -> walk seed' names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold seed' path'
                        case next of 
                            done@(Done _) -> return done
                            seed''        -> walk (unwrap seed'') names
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)

