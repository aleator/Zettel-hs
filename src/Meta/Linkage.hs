module Meta.Linkage where
import qualified Database.SQLite.Simple as SQL
import           Path
import           Zettel

createLinkageDB :: Path Abs File -> IO SQL.Connection
createLinkageDB dbPath = do
   conn <- SQL.open (toFilePath dbPath)
   SQL.execute_ conn "CREATE TABLE IF NOT EXISTS linkstructure\
                 \(id INTEGER PRIMARY KEY, fromZ TEXT, toZ TEXT, ref TEXT)"
   pure conn 

refreshLinks :: SQL.Connection -> Named Zettel -> IO ()
refreshLinks conn zettel = SQL.withTransaction conn <| do
    SQL.execute conn "DELETE FROM linkstructure \
                     \WHERE \
                     \fromZ = ?" (SQL.Only (name zettel))
    forM_ (namedValue zettel |> links) <| \lnk -> 
        SQL.execute conn "INSERT INTO linkstructure(fromZ,toZ,ref) VALUES (?,?,?)"
                         (name zettel,linkTarget lnk, refNo lnk)

findNeighbours :: SQL.Connection -> Text -> IO [Text]
findNeighbours conn zettelName = 
        fmap (map SQL.fromOnly) <| SQL.query conn
                    "with source(title) as ( \
                    \ values(?) \
                    \), \
                    \parents(title) as ( \
                    \  select distinct fromZ from linkstructure, source  where \
                    \     toZ = source.title \
                    \), \
                    \siblings(title) as ( \
                    \  select distinct toZ from linkstructure where \
                    \    fromZ in (select * from parents) \
                    \), \
                    \children(title) as ( \
                    \  select distinct toZ from linkstructure,source where \
                    \    fromZ = source.title \
                    \     \
                    \) \
                    \select distinct title from children UNION \
                    \select distinct title from siblings UNION \
                    \select distinct title from parents "
                    (SQL.Only zettelName)
                
findBacklinks :: SQL.Connection -> Text -> IO [Text]
findBacklinks conn zettelName = 
    fmap (map SQL.fromOnly) <| 
     SQL.query conn "select fromZ from linkstructure where toZ = ?" (SQL.Only zettelName)

findOriginThread :: SQL.Connection -> Text -> IO [Text]
findOriginThread conn zettelName =
     fmap (map SQL.fromOnly) <| SQL.query conn 
                    "with recursive cnt(x,n) AS ( \
                    \    values(?,1) \
                    \    union all \
                    \    select linkstructure.toZ,cnt.n+1 from linkstructure, cnt \
                    \    where  \
                    \        linkstructure.fromZ = cnt.x \
                    \        and  \
                    \        linkStructure.ref = \"Origin\" \
                    \        and cnt.n<2000 \
                    \  ) \
                    \  select distinct x from cnt order by n asc;"
                    (SQL.Only zettelName)

