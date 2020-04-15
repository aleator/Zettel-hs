{-#Language DerivingVia#-}
module Meta.History where

import qualified Database.SQLite.Simple as SQL
import           Path
import           Data.Time.Clock

createHistoryDB :: Path Abs File -> IO SQL.Connection
createHistoryDB dbPath = do
   conn <- SQL.open (toFilePath dbPath)
   SQL.execute_ conn "CREATE TABLE IF NOT EXISTS AccessLog \
                     \(id INTEGER PRIMARY KEY, event TEXT, zettel TEXT, date TEXT)"
   SQL.execute_ conn "CREATE TABLE IF NOT EXISTS TemporalRelation \
                     \(fromZ TEXT, toZ TEXT, count INTEGER, PRIMARY KEY (fromZ,toZ))"
   pure conn 

recordOpenZettel :: SQL.Connection -> Text -> IO ()
recordOpenZettel conn zettelName = do
    now <- getCurrentTime
    SQL.execute conn
                "INSERT INTO AccessLog(event,zettel,date) VALUES (?,?,?)"
                ("OPEN" :: Text, zettelName, now)
    SQL.execute conn
                "INSERT OR IGNORE INTO TemporalRelation \
                \SELECT ?, zettel, 1 FROM AccessLog \ 
                \WHERE date > ?"
                (zettelName,addUTCTime (15*60) now)
    let timeUpdate time = 
          SQL.execute conn
                    "UPDATE TemporalRelation \
                    \SET count = 1 + count \
                    \ WHERE fromZ = ? \
                    \ AND toZ in (SELECT zettel FROM AccessLog \
                    \              WHERE date > ?)" 
                    (zettelName, time)
    -- Update counts so that weights near the time are higher.
    addUTCTime (negate (15*60)) now |> timeUpdate
    addUTCTime (negate (5*60))  now |> timeUpdate 
    addUTCTime (negate (1*60))  now |> timeUpdate 
    addUTCTime (negate 30)      now |> timeUpdate 

newtype Minutes = Min Natural 
    deriving Num via Natural
    deriving Show via Natural
    deriving Read via Natural
    deriving Ord via Natural
    deriving Eq via Natural

recentZettels :: SQL.Connection -> Minutes -> IO [Text]
recentZettels conn (Min n) = do
    now <- getCurrentTime
    SQL.query conn
              "SELECT DISTINCT zettel from AccessLog WHERE date > ? ORDER BY date ASC"
              (SQL.Only (addUTCTime (- (fromIntegral n * 60)) now))
           >>= map SQL.fromOnly .> pure

temporalNeighbourhood :: SQL.Connection -> Natural -> Text -> IO [Text]
temporalNeighbourhood conn number zettelName = do
    SQL.queryNamed conn
              "SELECT DISTINCT fromZ, count from TemporalRelation WHERE toZ = :zettelName \ 
              \UNION \
              \SELECT DISTINCT toZ, count from TemporalRelation WHERE fromZ = :zettelName \ 
              \ORDER BY count DESC LIMIT :num"
              [":zettelName" SQL.:= zettelName, ":num" SQL.:= (fromIntegral number :: Int) ]
           >>= map (fst :: (Text,Int) -> Text) .> filter (/= zettelName) .> pure
