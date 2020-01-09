{-#language OverloadedStrings#-}
{-#language TemplateHaskell#-}
module Main where
import           Options.Applicative
import           System.Process.Typed
import           Parser -- TODO Make a Type module instead
import           Operations
import qualified Data.Text.Encoding            as T
import qualified Data.Text                     as T
import           Path
import           Data.List                      ( (\\) )
import           Path.IO                       as Dir


data Commands
  = AddLinks FilePath (Maybe Text)
  | Extend FilePath Text (Maybe Text)
  | BuildClique CliqueType (Maybe Text)
  | Find (Maybe Text)
  -- | UpdateIndex
  | Create Text CreateLinks
  | ExportAsJSON WhatToExport
  deriving (Eq, Show)

-- data HowToFind = KeywordSearch (Maybe Text

data WhatToExport = ExportAll | ExportSearch (Maybe Text)
    deriving (Eq,Show)

data CliqueType = CliqueZettel Text | CrossLink
    deriving (Eq,Show)

data CreateLinks = DontAddLinks | DoAddLinks | AddLinksKeyword Text
 deriving (Eq,Show)

cmdExport :: Parser Commands
cmdExport =
  ExportAsJSON
    <$> (   flag'
             ExportAll 
            (  long "all"
            <> help "Export all zettels"
            )
        <|> (   ExportSearch
            <$> optional (strOption
                  (  long "search"
                  <> metavar "KEYWORD"
                  <> help "Search for zettels to export"
                  ))
            )
        )
        

cmdClique :: Parser Commands
cmdClique =
  BuildClique
    <$> (   flag'
            CrossLink
            (  long "crosslink"
            <> help "Crosslink zettels directly without creating a new one"
            )
        <|> (   CliqueZettel
            <$> strOption
                  (  long "title"
                  <> help "Title for zettel describing the clique"
                  )
            )
        )
    <*> optional
          (strArgument
            (help "Search term for selecting Clique members" <> metavar "KEYWORD"
            )
          )

cmdCreate :: Parser Commands
cmdCreate =
  Create
    <$> strOption
          (long "title" <> help "Title for the new zettel" <> metavar "ZETTEL")
    <*> (   (AddLinksKeyword <$> strOption
              (  long "search"
              <> help "Search for links to add to the new zettel"
              <> metavar "KEYWORD"
              )
            )
        <|> (flag DontAddLinks
                  DoAddLinks
                  (long "dolink" <> help "Add links without searching")
            )
        )

cmdAddLinks :: Parser Commands
cmdAddLinks =
  AddLinks
    <$> strOption
          (long "origin" <> help "Zettel to add links to" <> metavar "ZETTEL")
    <*> optional
          (strOption (long "search" <> short 's' <> metavar "SEARCH_TERM"))

cmdExtend :: Parser Commands
cmdExtend =
  Extend
    <$> strOption (long "origin" <> metavar "ZETTEL" <> help "Source zettel")
    <*> strOption
          (long "title" <> metavar "NEW_TITLE" <> help
            "Title for the new zettel"
          )
    <*> optional (strOption (long "relation" <> short 'r' <> metavar "ZETTEL"))


cmdFind :: Parser Commands
cmdFind = Find
  <$> optional (strArgument (metavar "KEYWORD" <> help "Keyword to search"))


cmdCommands :: Parser Commands
cmdCommands =
  subparser
      (command
        "create"
        (info (cmdCreate <**> helper) (progDesc "Create unlinked zettel"))
      )
    <|> subparser
          (command "link"
                   (info (cmdAddLinks <**> helper) (progDesc "Link zettels"))
          )
    <|> subparser
          (command
            "extend"
            (info (cmdExtend <**> helper)
                  (progDesc "Create new zettel and link it to original")
            )
          )
    <|> subparser
          (command "find" (info (cmdFind <**> helper) (progDesc "Find zettels"))
          )
    <|> subparser
          (command
            "clique"
            (info (cmdClique <**> helper)
                  (progDesc "Build cliques by cross linking selected zettels")
            )
          )
    <|> subparser
          (command
            "export"
            (info (cmdExport <**> helper)
                  (progDesc "Export zettels as JSON")
            )
          )


main :: IO ()
main = do
  cmdOpts <- execParser
    (info
      (cmdCommands <**> helper)
      (fullDesc <> progDesc "Manipulate zettelkasten" <> header
        "ZKHS -- simple text based zettelkasten system"
      )
    )
  home <- getHomeDir
  let zettelkasten = fileSystemZK (home </> $(mkRelDir "zettel"))
  case cmdOpts of
    AddLinks origin maybeSearch -> do
      zettel   <- loadZettel zettelkasten (toText origin)
      theLinks <- keywordSearch zettelkasten maybeSearch
      addLinks theLinks <$> zettel |> saveZettel zettelkasten
      pass
    Extend origin newTitle maybeRelation -> do
      original                    <- loadZettel zettelkasten (toText origin)
      (modifiedOriginal, created) <- createLinked original
                                                  maybeRelation
                                                  newTitle
      saveZettel zettelkasten modifiedOriginal
      saveZettel zettelkasten created

    Find maybeKeyword -> do
      links <- keywordSearch zettelkasten maybeKeyword
      traverse_ (linkToFile zettelkasten >=> toFilePath .> putStrLn) links

    BuildClique cliqueType maybeKeyword -> do
      links <- keywordSearch zettelkasten maybeKeyword
      case cliqueType of
        CrossLink -> do
          for_ links $ \lnk -> do
            zettel <- loadZettel zettelkasten (linkTarget lnk)
            fmap (addLinks (filter (/= lnk) links)) zettel
              |> saveZettel zettelkasten

        CliqueZettel title -> do
          cliqueZettel <- create title
          fmap (addLinks links) cliqueZettel |> saveZettel zettelkasten
          for_ links $ \lnk -> do
            linkedZettel <- loadZettel zettelkasten (linkTarget lnk)
            fmap (addLinks [Link (name cliqueZettel) (Just "Clique link") Nothing])
                 linkedZettel
              |> saveZettel zettelkasten
            linkToFile zettelkasten (linkTo cliqueZettel) >>= toFilePath .> putStrLn

    Create title doAddLinks -> do
      zettel <- create title
      lnks   <- case doAddLinks of
        DontAddLinks       -> pure Nothing
        DoAddLinks         -> Just <$> keywordSearch zettelkasten Nothing
        AddLinksKeyword kw -> Just <$> keywordSearch zettelkasten (Just kw)
      saveZettel zettelkasten $ case lnks of
            Nothing -> zettel
            Just someLinks -> addLinks someLinks <$> zettel
      linkToFile zettelkasten (linkTo zettel) >>= toFilePath .> putStrLn

    ExportAsJSON whatToExport -> do
        links <- case whatToExport of
            ExportAll -> listZettels zettelkasten
            ExportSearch maybeKeyword -> 
                keywordSearch zettelkasten maybeKeyword
        -- TODO: Note that this is object/line format
        for_ links $ \lnk -> do
            zettel <- loadZettel zettelkasten (linkTarget lnk)
            putLTextLn (exportAsJSON zettel)
                

-- UTILS

data ZettelKasten = ZettelKasten
    {
     saveZettel    :: Named Zettel -> IO ()
    ,loadZettel    :: Text -> IO (Named Zettel)
    ,keywordSearch :: Maybe Text -> IO [Link]
    ,linkToFile    :: Link -> IO (Path Abs File)
    ,listZettels   :: IO [Link]
    }

fileSystemZK basedir = ZettelKasten
  (\(Named n zettel) -> do
    p <- parseRelFile (toString n)
    writeZettel (basedir </> filename p) zettel
  )
  (\uuid -> Named uuid <$> readZettel uuid)
  (rgFind basedir)
  (fileSystemLinkToFile basedir)
  (findZettelFiles basedir)

findZettelFiles basedir = do
  (_,files) <- listDir basedir

  let filePathToLink = filename .> toFilePath .> toLink
      toLink ident = Link (toText ident) Nothing Nothing

  pure [filePathToLink f | f <- files
       ,not ("."` isPrefixOf` toFilePath (filename f))] 

fileSystemLinkToFile baseDir (Link lnk _ _) = do
  file <- parseRelFile (toString lnk)
  pure (baseDir </> file)

writeZettel :: Path Abs File -> Zettel -> IO ()
writeZettel n z = writeFileText (toFilePath n) (pprZettel z)

rgFind zettelkastendir maybeSearch = withCurrentDir zettelkastendir <| do
  let rgOpts = case maybeSearch of
        Nothing      -> ["-l", "."]
        Just keyword -> ["-l", toString keyword]
      fzfOpts = ["--multi", "--preview", "cat {}"]
  (ec, out) <- withProcessTerm_
    (proc "rg" rgOpts |> setStdout createPipe)
    (\p ->
      proc "fzf" fzfOpts
        |> setStdin (getStdout p |> useHandleClose)
        |> readProcessStdout
    )
  let filePathToLink fp = case parseRelFile (toString fp) of
        Nothing   -> Nothing
        Just path -> path |> filename |> toFilePath |> Just
  pure
    [ Link (toText lnk) Nothing Nothing
    | lnk <- toStrict out |> decodeUtf8 |> lines |> mapMaybe filePathToLink
    ]

-- TODO: Use proper paths
readZettel :: Text -> IO Zettel
readZettel uuid = do
  txt <- readFileText ("/Users/aleator/zettel/" <> toString uuid)
  case runZettelParser (toString uuid) txt of
    Left  err -> error (toText err)  -- TODO: Raise proper exception
    Right r   -> pure r
