{-#LANGUAGE TypeApplications#-}
{-#LANGUAGE LambdaCase#-}
module ZettelKasten where

import Path
import Path.IO
import           System.Process.Typed
import qualified Data.HashSet                  as HashSet
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LT
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Data.ByteString.Lazy          as LBS

import Zettel 
import Parser(runZettelParser)

type SearchResults = SearchResultsParam [Link]
data SearchResultsParam a = CreateNew Text a
                          | Links a

data ZettelKasten = ZettelKasten
    {
     saveZettel    :: Named Zettel -> IO ()
    ,loadZettel    :: Text -> IO (Named Zettel)
    ,keywordSearch :: Maybe Text -> IO SearchResults
    ,linkToFile    :: Link -> IO (Path Abs File)
    ,listZettels   :: IO [Link]
    }

fileSystemZK basedir = ZettelKasten
  (\(Named n zettel) -> do
    p <- parseRelFile (toString n)
    writeZettel (basedir </> filename p) zettel
  )
  (\uuid -> Named uuid <$> readZettel basedir uuid)
  (rgFind basedir)
  (fileSystemLinkToFile basedir)
  (findZettelFiles basedir)

findZettelFiles basedir = do
  (_, files) <- listDir basedir

  let filePathToLink = filename .> toFilePath .> toLink
      toLink ident = Link (toText ident) Nothing Nothing

  pure
    [ filePathToLink f
    | f <- files
    , not ("." `isPrefixOf` toFilePath (filename f))
    ]

fileSystemLinkToFile baseDir (Link lnk _ _) = do
  file <- parseRelFile (toString lnk)
  pure (baseDir </> file)

writeZettel :: Path Abs File -> Zettel -> IO ()
writeZettel n z = writeFileText (toFilePath n) (pprZettel z)
      
askForLabels :: ZettelKasten -> LinkStructure -> Link -> IO Link
askForLabels zettelkasten linkStructure (Link linkTarget desc ref) = do
        title <- loadZettel zettelkasten linkTarget >>= namedValue .> title .> pure
        lbl <-
          findLabelsFor linkStructure linkTarget
          >>= (title :)
          .>  filter (/= "")
          .>  filter (/= "Origin")
          .>  ordNub
          .>  selectLabels

        pure (Link linkTarget desc (Just lbl))

fzf inputPipe 
 = let
      fzfOpts = ["--multi"
                ,"-d","-","--with-nth","6.."
                ,"--print-query","--expect=ctrl-n"
                , "--preview", "Zettel body --origin {}"]
   in proc "fzf" fzfOpts |> setStdin inputPipe -- (getStdout p |> useHandleClose)

findLabelsFor ::  LinkStructure ->  Text -> IO [Text]
findLabelsFor linkStructure source = do
    case HashMap.lookup source (hasLinkFrom linkStructure) of
        Nothing -> pure []
        Just linkSet -> pure [label | linker <- toList linkSet
                             , label <- HashMap.lookup (linker,source) (linkLabel linkStructure) 
                                         |> maybe [] toList]

selectLabels :: [Text] -> IO Text
selectLabels labels 
 = let
      fzfOpts = [
                 "--print-query","--expect=ctrl-n"
                 ,"--header=press ctrl-n to create new"
                ]
      encodedLabels = LT.unlines (map toLText labels) |> LT.encodeUtf8
   in do
      out <- proc "fzf" fzfOpts |> setStdin (byteStringInput encodedLabels)
                         |> readProcessStdout @IO |> fmap snd
                         |> fmap (toStrict .> decodeUtf8 .> lines .> ordNub) 
      case out of
          "":label:[] -> pure label         -- WAT? Simply selecting one
          label:"":[] -> pure label         -- No results, no ctrl-n
          query:"ctrl-n":_ -> pure query    -- ctrl-n
          _:"":result:[] -> pure result         -- a result
          other -> errorExit ("Cannot understand fzf result: "<>show other::LText)

p1 --> p2 = withProcessTerm_ (p1 |> setStdout createPipe) (getStdout .> useHandleClose .> p2)

rgFind zettelkastendir maybeSearch = withCurrentDir zettelkastendir <| do
  let rgOpts = case maybeSearch of
        Nothing      -> ["-l", "."]
        Just keyword -> ["-l", toString keyword]
  (ec,out) <- proc "rg" rgOpts --> (fzf .> readProcessStdout)
  parseFZFOutput out 

parseFZFOutput :: LBS.ByteString -> IO SearchResults
parseFZFOutput out = do
  let filePathToLink fp = case parseRelFile (toString fp) of
        Nothing   -> Nothing
        Just path -> path |> filename |> toFilePath |> Just
  let pathsToLinks fzfResults 
        = [ Link (toText lnk) Nothing Nothing
          | lnk <- mapMaybe filePathToLink fzfResults ] 

  case toStrict out |> decodeUtf8 |> lines of
            query:"ctrl-n":searchResults 
                        -> CreateNew query (pathsToLinks searchResults) |> pure
            query:"":[] -> CreateNew query [] |> pure
            _:_:searchResults -> Links (pathsToLinks searchResults) |> pure
            x -> errorExit ("Cannot understand fzf result: "<>show x::LText)


errorExit msg = LT.hPutStrLn stderr (toLText msg) >> Prelude.exitFailure

-- TODO: Use proper paths
readZettel :: Path Abs Dir -> Text -> IO Zettel
readZettel path uuid = do
  fpUUID <- parseRelFile (toString uuid)
  txt    <- readFileText (toFilePath (path </> fpUUID))
  case runZettelParser (toString uuid) txt of
    Left  err -> error (toText err)  -- TODO: Raise proper exception
    Right r   -> pure r

data LinkStructure = 
    LS {linksTo, hasLinkFrom :: HashMap Text (HashSet Text)
       ,linkLabel :: HashMap (Text,Text) (HashSet Text)}

instance Semigroup LinkStructure where
    (<>) (LS to₁ from₁ lbl₁) (LS to₂ from₂ lbl₂) 
        = LS (to₁<*>to₂) (from₁<*>from₂) (lbl₁ <*> lbl₂)
     where a <*> b = HashMap.unionWith (<>) a b

instance Monoid LinkStructure where
    mempty = LS mempty mempty mempty

scanZettelKasten :: Monoid m => ZettelKasten -> (Named Zettel -> m) -> IO m
scanZettelKasten zettelkasten op = do
  zettels <- listZettels zettelkasten
  -- TODO: This could be concurrent 
  flip foldMap zettels $ \zettelLink -> do
    zettel <- loadZettel zettelkasten (linkTarget zettelLink)
    op zettel |> pure

-- Get link structure out of linked zettels
getLinkStructure :: ZettelKasten -> [Link] -> IO LinkStructure
getLinkStructure zettelkasten zettels = 
  flip foldMap zettels $ \zettelLink -> do
    zettel <- loadZettel zettelkasten (linkTarget zettelLink)
    linkExtraction zettel |> pure 

linkExtraction :: Named Zettel -> LinkStructure
linkExtraction zettel = 
    let
      theLinks  = namedValue zettel |> links |> map linkTarget
      lnksTo    = HashMap.singleton (name zettel) (HashSet.fromList theLinks)
      lnksFrom  = HashMap.fromList
        (zip theLinks (repeat (name zettel |> HashSet.singleton)))
      lnkLabels = HashMap.fromList
                  [((name zettel,target), HashSet.singleton label)
                  | Link target desc ref <- namedValue zettel |> links
                  , label <- maybeToList (ref <|> desc)]
    in LS lnksTo lnksFrom lnkLabels

-- Find unlinked zettels
unlinked :: LinkStructure -> HashSet Text
unlinked (LS thisLinksTo thisHasLinkFrom _) =
  foldMap 
    (\zettelLink -> case HashMap.lookup zettelLink thisHasLinkFrom of
                      Nothing -> HashSet.singleton zettelLink
                      Just _ -> mempty
    )
    (HashMap.keys thisLinksTo) -- (map linkTarget links)

