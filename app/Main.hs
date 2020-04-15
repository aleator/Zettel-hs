{-OPTIONS_GHC -Werror=incomplete-patterns-}
{-#language OverloadedStrings#-}
{-#language LambdaCase#-}
{-#language TupleSections#-}
{-#language ScopedTypeVariables#-}
{-#language TemplateHaskell#-}
{-#language DeriveAnyClass#-}
{-#language DerivingVia#-}
{-#language DeriveGeneric#-}
{-#language BlockArguments#-}
module Main where
import           Options.Applicative
import           System.Process.Typed

import           Zettel -- TODO Make a Type module instead
import           Operations
import           Elucidations
import           Visualization
import           ZettelKasten
import           Meta.History
import           Meta.Linkage

import           Data.Tree
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.IO             as LT
import qualified Data.Text.IO                  as T
import qualified Data.Char                     
import qualified Data.CaseInsensitive          as CI
import           Data.Time.Clock
import           Path
import           Path.IO                       as Dir
import           Data.FileEmbed                (embedFile)
import           System.IO                     (hClose)
import qualified Data.ByteString.Lazy.Char8    as Char8
import           Control.Exception
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Data.HashSet (HashSet)
import qualified Data.Foldable as F

-- For elucidate
import qualified Numeric.Sampling 
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL

import qualified Data.Aeson                    as Aeson
-- TODO: Move tantify stuff to it's own file


data Commands
  = AddLinks FilePath (AutoMaybe Text Text) (Maybe Text)
  | Find (Maybe Text) HowToFind
  | Create Text (Maybe Text) (Maybe Text) CreateLinks InitialContent
  | ResolveReference ResolveMissing Text Text
  | ExportAsJSON WhatToExport
  | Neighbourhood ForWho ChooseFrom
  -- | Thread Text
  | Body Text
  | References Text
  | AddReferences Text (Target BibItem)
  | Elucidate
  | FillLabels Text
  | Touch TouchType Text
  deriving (Eq, Show)

data TouchType = TouchOpen deriving (Eq,Show)

data AutoMaybe opts a = Auto opts | No | Use a deriving (Eq,Show,Read,Generic)

data ForWho = Human | Computer deriving (Show,Eq)

data InitialContent = NoInitialContent | YesInitialContent deriving (Eq,Show)

data Target a = This a | Choose ChooseFrom deriving (Eq,Show)
data ChooseFrom = AllZettels 
                | FromNeighbourhood Text 
                | FromOriginThread  Text 
                | FromOriginTree    Text 
                | FromOutbound      Text 
                | FromBackLinks     Text 
                | FromRecent        Minutes
                | FromTemporal      Natural Text 
                deriving (Eq,Show)

data ResolveMissing = CreateNewByRefID | ReturnError
    deriving (Eq,Show)

data HowToFind = KeywordSearch Text | FuzzyFindAll | FullTextSearch TantivySearchStyle Text
    deriving (Eq,Show)

data WhatToExport = ExportAll | ExportSearch (Maybe Text)
    deriving (Eq,Show)

data CreateLinks = DontAddLinks | DoAddLinks | AddLinksKeyword Text
 deriving (Eq,Show)

argChooseFrom :: Parser ChooseFrom
argChooseFrom = 
    (flag' AllZettels (long "all-zettels" <> help "Select all zettels"))
    <|>
    (FromNeighbourhood 
        <$> strOption (long "neighbourhood" <> help "Select neighbourhood of this zettel" <> metavar "ZETTEL"))
    <|>
    (FromOriginThread 
        <$> strOption (long "thread" <> help "Select origin thread of this zettel" 
                                     <> metavar "ZETTEL"))
    <|>
    (FromOriginTree
        <$> strOption (long "tree" <> help "Select origin tree of this zettel" 
                                     <> metavar "ZETTEL"))
    <|>
    (FromBackLinks
        <$> strOption (long "backlinks" <> help "Select backlinks to zettel" 
                                     <> metavar "ZETTEL"))
    <|>
    (FromOutbound
        <$> strOption (long "outbound" <> help "Select backlinks to zettel" 
                                     <> metavar "ZETTEL"))
    <|>
    (FromTemporal
        <$> option auto (long "count" <> help "Number of zettels select" 
                                     <> metavar "NATURAL")
        <*> strOption (long "temporal" <> help "Select temporally related zettels" 
                                     <> metavar "ZETTEL")
                                     )
    <|>
    (FromRecent
        <$> option auto (long "recent" <> help "Select recently edited zettels" 
                                        <> metavar "MINUTES"))

cmdExport :: Parser Commands
cmdExport =
  ExportAsJSON
    <$> (   flag' ExportAll (long "all" <> help "Export all zettels")
        <|> (ExportSearch <$> optional
              (strOption
                (long "search" <> metavar "KEYWORD" <> help
                  "Search for zettels to export"
                )
              )
            )
        )


--cmdClique :: Parser Commands
--cmdClique =
--  BuildClique
--    <$> (   flag'
--            CrossLink
--            (  long "crosslink"
--            <> help "Crosslink zettels directly without creating a new one"
--            )
--        <|> (   CliqueZettel
--            <$> strOption
--                  (  long "title"
--                  <> help "Title for zettel describing the clique"
--                  )
--            )
--        )
--    <*> optional
--          (strArgument
--            (  help "Search term for selecting Clique members"
--            <> metavar "KEYWORD"
--            )
--          )

cmdCreate :: Parser Commands
cmdCreate =
  Create
    <$> strOption
          (long "title" <> help "Title for the new zettel" <> metavar "ZETTEL")
    <*> optional (strOption
          (long "origin" <> help "Optional Origin zettel for the created one" <> metavar "ZETTEL"))
    <*> optional (strOption
          (long "ref-id" <> help "How to refer to this zettel in the origin, if given" <> metavar "ZETTEL"))
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
    <*> (flag' YesInitialContent (long "initial"<>help "Add initial content from stdin")
        <|> pure NoInitialContent)

cmdBody :: Parser Commands
cmdBody =
  Body
    <$> strOption
          (long "origin" <> help "Zettel from which to extract body from" <> metavar "ZETTEL")


cmdReferences :: Parser Commands
cmdReferences =
  References
    <$> strOption
          (long "origin" <> help "Zettel from which to extract references from" <> metavar "ZETTEL")

cmdAddReferences :: Parser Commands
cmdAddReferences =
  AddReferences
    <$> strOption
          (long "origin" <> help "Zettel which to add references to" <> metavar "ZETTEL")
    <*> (Choose <$>
            (flag' AllZettels 
                    (long "all" <> help "Choose from all known references")
            <|> 
            (FromNeighbourhood <$> 
                strOption 
                    (long "neighbourhood" <> help "Choose from neighbourhood of this zettel"<> metavar "ZETTEL")))
        <|>
        (This <$> (BibItem <$> strOption (long "key" <> help "Add with this key"<>metavar "REFID") <*> strOption 
                    (long "reference"<> help "Add this reference"
                            <> metavar "REFERENCE")))
            )



cmdNeighbourhood :: Parser Commands
cmdNeighbourhood = 
    Neighbourhood 
      <$> (flag Computer Human 
            (long "human"<>help "Human readable output where sensible"))
      <*> argChooseFrom
  --  <$> strOption
  --        (long "origin" <> help "Zettel from which to search" <> metavar "ZETTEL")

cmdResolveReference :: Parser Commands
cmdResolveReference =
  ResolveReference
    <$> flag ReturnError CreateNewByRefID 
          (long "create" <> help "If the reference doesn't point anywhere, create a new zettel and link to it")
    <*> strOption
          (long "origin" <> help "Zettel containing a reference" <> metavar "ZETTEL")
    <*> strOption
          (long "reference-text" <> short 'r' <> metavar "REFERENCE")


cmdAddLinks :: Parser Commands
cmdAddLinks =
  AddLinks
    <$> strOption
          (long "origin" <> help "Zettel to add links to" <> metavar "ZETTEL")
    <*> (    (flag' Auto (long "ask"<>help "Ask for labels")
                <*> strOption (long "placeholder"<>help "Put references at placeholder text"))
         <|> (Use <$> strOption (long "reference" <> short 'r' <> metavar "REFERENCE"))
         <|> pure No)
    <*> optional
          (strOption (long "search" <> short 's' <> metavar "SEARCH_TERM"))

--cmdExtend :: Parser Commands
--cmdExtend =
--  Extend
--    <$> strOption (long "origin" <> metavar "ZETTEL" <> help "Source zettel")
--    <*> strOption
--          (long "title" <> metavar "NEW_TITLE" <> help
--            "Title for the new zettel"
--          )
--    <*> optional (strOption (long "ref-id" <> short 'r' <> metavar "Reference id"))
--    <*> optional (strOption (long "relation" <> short 'r' <> metavar "ZETTEL"))
-- Consider dropping relation entirely


cmdFind :: Parser Commands
cmdFind =
  Find
    <$>
     optional (strOption
       (long "origin" <> help "Zettel to add origin link to, if search creates a new zettel" <> metavar "ZETTEL")
        )
    <*> (   (   KeywordSearch
            <$> (strArgument (metavar "KEYWORD" <> help "Keyword to search"))
            )
        <|> (   FullTextSearch
            <$> (flag
                  RebuildIndex
                  UseExistingIndex
                  (  long "fast"
                  <> help
                       "Skip rebuilding the index (use this if no changes have been made"
                  )
                )
            <*> (strOption
                  (long "query" <> short 'q' <> help
                    "Full text query (see tantivy options)"
                  )
                )
            )
        <|> pure FuzzyFindAll
        )

-- cmdThread :: Parser Commands
-- cmdThread =
--   Thread
--     <$> strOption (long "origin" <> metavar "ZETTEL" <> help
--                   "Zettel to start following Origin thread from"
--                 )

cmdFillLabels :: Parser Commands
cmdFillLabels =
  FillLabels
    <$> strOption (long "target" <> metavar "ZETTEL" <> help
                  "Zettel to try to autofill labels for"
                )

cmdTouch :: Parser Commands
cmdTouch =
  Touch
    <$> flag' TouchOpen (long "open" <> help "Call when opening a zettel")
    <*> strOption (long "target" <> metavar "ZETTEL" <> help
                  "Zettel to try to autofill labels for"
                )

--TODO, BUG: IF rg does not find anything, fzf is launched empty and error is printed
--

cmdCommands :: Parser Commands
cmdCommands = subparser
  (  cmd cmdCreate   "create" "Create unlinked zettel"
  <> cmd cmdAddLinks "link"   "Link zettels"
  -- <> cmd cmdExtend   "extend" "Create new zettel and link it to original"
  <> cmd cmdFind     "find"   "Find zettels"
  <> cmd cmdResolveReference "resolve" "Resolve references in zettels"
  -- <> cmd cmdClique "clique" "Build cliques by cross linking selected zettels"
  <> cmd cmdExport   "export" "Export zettels as JSON"
  <> cmd (pure Elucidate) "elucidate" "Suggest improvements in ZettelKasten"
  <> cmd cmdNeighbourhood "neighbourhood" "Zettels linkwise near to this one"
  <> cmd cmdBody "body" "Extract zettel body, ie. text without headers and links"
  <> cmd cmdReferences "references" "Extract references from a Zettel"
  <> cmd cmdAddReferences "addreferences" "Add references to a Zettel"
--  <> cmd cmdThread "thread" "Compute the transitive origin of a Zettel"
  <> cmd cmdFillLabels "auto-fill" "Fill missing wikilinks and references from origin"
  <> cmd cmdTouch "touch" "Record opening a zettel (for logging purposes)"
  )
 where
  cmd theCmd name desc =
    command name (info (theCmd <**> helper) (progDesc desc))



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
  let linkageDBFile     = home </> $(mkRelFile "zettel/.linkage.db")--TODO: Wrap this like the zettelkasten is wrapped
  zettelkasten <- fileSystemZK (home </> $(mkRelDir "zettel")) 
                    |> setAutoStoreLinkage linkageDBFile
  let indexDir     = home </> $(mkRelDir "zettel/.zettel_index")--TODO: Wrap this like the zettelkasten is wrapped
  let metaDB       = home </> $(mkRelFile "zettel/.meta_db")--TODO: Wrap this like the zettelkasten is wrapped

  case cmdOpts of

    AddLinks origin maybeReference maybeSearch -> do
      zettel   <- loadZettel zettelkasten (toText origin)
      searchResults <- keywordSearch zettelkasten maybeSearch
      let askForLabels theLinks = do 
                        zettels <- listZettels zettelkasten
                        links <- getLinkStructure zettelkasten zettels
                        let getLabelFor (Link linkTarget desc ref) = do
                                title <- loadZettel zettelkasten linkTarget
                                          >>= namedValue .> title .> pure
                                lbl <- findLabelsFor links linkTarget 
                                    >>= (title:)
                                        .> filter (/="") 
                                        .> filter (/="Origin")
                                        .> ordNub 
                                        .> selectLabels
                                            
                                pure (Link linkTarget desc (Just lbl))
                        labeled <- traverse getLabelFor theLinks 
                        pure labeled

      case searchResults of
        Links theLinks -> case maybeReference of
                           No      -> addLinks theLinks <$> zettel |> saveZettel zettelkasten
                           Use ref -> addLinks (map (addRefId ref) theLinks) <$> zettel |> saveZettel zettelkasten
                           Auto placeholder   -> do
                                        labeledLinks <- askForLabels theLinks 
                                        let labels = T.intercalate " ,"
                                                     [ pprLabel rn | Link _ _ (Just rn) <- labeledLinks ]
                                        saveZettel zettelkasten 
                                            (replacePlaceholder (Placeholder placeholder)
                                                                labels 
                                             <$> addLinks labeledLinks 
                                             <$> zettel)
                                        
        CreateNew title links  -> do
            original <- loadZettel zettelkasten (toText origin)
            (newOriginal,newZettel) <- createLinked original Nothing Nothing title
            fmap (addLinks links) newZettel |> saveZettel zettelkasten
            labeledLinks <- askForLabels [Link (name newZettel) Nothing Nothing]
            saveZettel zettelkasten (addLinks labeledLinks <$> newOriginal)
            printLabels labeledLinks
      pass

    Find mOrigin howToFind -> do
      searchResults <- case howToFind of
          FuzzyFindAll          -> keywordSearch zettelkasten Nothing
          KeywordSearch keyword ->  keywordSearch zettelkasten (Just keyword)
          FullTextSearch tantivyOptions query -> Links <$> 
            doTantivySearch zettelkasten indexDir 
                                                  tantivyOptions query
      case searchResults of
        CreateNew title links -> do
            zettel <- case mOrigin of
                Nothing -> create title
                Just origin -> do
                    original <- loadZettel zettelkasten (toText origin)
                    (newOriginal,newZettel) <- createLinked original Nothing Nothing
                                             title
                    saveZettel zettelkasten newOriginal
                    return newZettel
            fmap (addLinks links) zettel |> saveZettel zettelkasten
            linkToFile zettelkasten (linkTo zettel) >>= toFilePath .> putStrLn
        Links links -> traverse_ (linkToFile zettelkasten >=> toFilePath .> putStrLn) links

    Create title mOrigin mRefId doAddLinks addInitialContent -> do
      initialContent <- case addInitialContent of
                         NoInitialContent -> pure ""
                         YesInitialContent -> T.getContents
      
      (modifiedOriginal, created) <- 
        case mOrigin of
            Nothing -> (Nothing,) <$> create title
            Just origin -> do
                original <- loadZettel zettelkasten origin 
                first Just <$> createLinked original
                                             mRefId
                                             Nothing
                                             title
      traverse_ (saveZettel zettelkasten) modifiedOriginal
      
      let zettel' = fmap (\z->z{body=initialContent}) created
      zettel <- fillMissingLinks zettelkasten zettel'
      
      searchResults   <- case doAddLinks of
        DontAddLinks       -> pure Nothing
        DoAddLinks         -> Just <$> keywordSearch zettelkasten Nothing
        AddLinksKeyword kw -> Just <$> keywordSearch zettelkasten (Just kw)

      saveZettel zettelkasten =<< case searchResults of
        Nothing        -> pure zettel
        Just (CreateNew _ _) -> errorExit ("Create cannot create two zettels?"::LText)
        Just (Links someLinks) -> pure (addLinks someLinks <$> zettel)

      linkToFile zettelkasten (linkTo zettel) >>= toFilePath .> putStrLn

    ResolveReference resolveMissing zettelID reference -> do
        zettel <- loadZettel zettelkasten zettelID
        let matches = [ lnk | lnk@(Link _ _ (Just refId)) <- links (namedValue zettel)
                            , CI.mk refId == CI.mk reference ]
        case matches of
            [singularLink] -> printLink zettelkasten singularLink 
            manyLinks@(_:_) -> traverse_ (printLink zettelkasten) manyLinks
            []    -> do
                          (modifiedOriginal, created) <- createLinked zettel
                                                                      (Just reference)
                                                                      Nothing
                                                                      reference
                          saveZettel zettelkasten modifiedOriginal
                          saveZettel zettelkasten created
                          let linkToCreated = Link (name created) Nothing (Just reference)
                          printLink zettelkasten linkToCreated

    ExportAsJSON whatToExport -> do
      links <- case whatToExport of
        ExportAll                 -> listZettels zettelkasten
        ExportSearch maybeKeyword -> keywordSearch zettelkasten maybeKeyword >>= \case
            Links lnks -> pure lnks
            CreateNew _ _ -> errorExit ("Export cannot create links" :: LText)
      -- TODO: Note that this is object/line format
      for_ links $ \lnk -> do
        zettel <- loadZettel zettelkasten (linkTarget lnk)
        putLTextLn (exportAsJSON zettel)

    Elucidate -> do
        conn <- createElucidationDB metaDB
        elucidations_ <- elucidate zettelkasten
        elucidations <- forM elucidations_ $ \(w,elucidation) -> do
            w2 <- getStoredWeight conn w (hashElucidation elucidation)
            pure (w2,(w2,elucidation))

        sample <- Numeric.Sampling.psampleIO 
                    (min 5 (length elucidations)) 
                    elucidations


        case sample of
         Nothing -> pure ()
         Just sampled -> do
            prettyPrint (map snd sampled )|> putTextLn
            decreaseElucidationPs conn 0.7 (map (second hashElucidation) sampled)

    Neighbourhood forWho area -> 
        let asLink :: Text -> Link
            asLink path = Link path Nothing Nothing
        in case area of
            FromNeighbourhood zettelName -> do
                conn <- createLinkageDB linkageDBFile
                neighbours <- findNeighbours conn zettelName
                traverse_ ( asLink .> printLink zettelkasten) neighbours

            FromOriginThread zettelName -> do
                conn <- createLinkageDB linkageDBFile
                findOriginThread conn zettelName >>= 
                    traverse_ (asLink .> printLink zettelkasten) 

            FromOriginTree zettelName -> do
                allZettels <- listZettels zettelkasten
                linkStructure <- getLinkStructure zettelkasten allZettels
                let originT = originTree linkStructure zettelName
                case forWho of
                    Human -> fmap toText originT |> drawUnicode 
                                |> traverse_ putTextLn 
                    Computer -> traverse_ 
                                    (asLink .> printLink zettelkasten) 
                                    originT

            FromOutbound zettelName -> do
                zettel <- readZettelFromNameOrFilename zettelkasten zettelName
                namedValue zettel |> links |> traverse_ (printLink zettelkasten) 

            FromBackLinks zettelName -> do
                conn <- createLinkageDB linkageDBFile
                findBacklinks conn zettelName >>=
                    traverse_ (asLink .> printLink zettelkasten)

            FromRecent min -> do
                c <- createHistoryDB metaDB
                recentZettels c min >>=
                    traverse_ (asLink .> printLink zettelkasten) 

            FromTemporal number zettelName -> do
                c <- createHistoryDB metaDB
                temporalNeighbourhood c number zettelName >>=
                    traverse_ (asLink .> printLink zettelkasten) 

            AllZettels -> do
                allZettels <- listZettels zettelkasten
                mapM_ (printLink zettelkasten) allZettels


    --Thread zettelName -> do
    --    -- Assume that there is only a single origin to keep this simple.
    --    origins <- originChain zettelkasten zettelName
    --    traverse_ (printLink zettelkasten) origins

    Body zettelNameOrFilename -> do
        zettel <- readZettelFromNameOrFilename zettelkasten zettelNameOrFilename
        namedValue zettel |> body |> putTextLn

    References zettelNameOrFilename -> do
        zettel <- readZettelFromNameOrFilename zettelkasten zettelNameOrFilename
        namedValue zettel 
            |> references 
            |> traverse_ (pprBib 
                          .> T.map (\x -> if x=='\n' then ' ' else x) 
                          .> putTextLn)


    FillLabels zettelName -> do
        -- Assume that there is only a single origin to keep this simple.
        zettel <- loadZettel zettelkasten zettelName
        fillMissingLinks zettelkasten zettel >>= saveZettel zettelkasten

    Touch TouchOpen zettelName -> do
        conn <- createHistoryDB metaDB
        recordOpenZettel conn zettelName


readZettelFromNameOrFilename zettelkasten zettelNameOrFilename =
  case parseAbsFile (toString zettelNameOrFilename) of
    Just zettelFile ->
      loadZettel zettelkasten (filename zettelFile |> toFilePath |> toText)
    Nothing -> loadZettel zettelkasten zettelNameOrFilename

printLink zettelkasten link = linkToFile zettelkasten link >>= toFilePath .> putStrLn
-- errorExit msg = LT.hPutStrLn stderr (toLText msg) >> exitFailure

-- Tantivy related things

data TantivySearchStyle = RebuildIndex | UseExistingIndex
    deriving (Show,Eq)

doTantivySearch
  :: ZettelKasten
  -> Path Abs Dir
  -> TantivySearchStyle
  -> Text
  -> IO [Link]
doTantivySearch zettelkasten basedir style query = do
  let indexDir = basedir </> $(mkRelDir ".zettel_index") 
  
  thereIsAnIndex <- doesDirExist indexDir
  when (not thereIsAnIndex || style == RebuildIndex)  <| do
    tantivySetupIndex indexDir
    tantivyBuildIndex zettelkasten indexDir

  tantivySearch indexDir query

  

tantivySetupIndex indexDir = do
  removeDirRecur indexDir `catch` (\(e::IOException) -> pure () )
  createDirIfMissing False indexDir 
  writeFileBS (toFilePath (indexDir</> $(mkRelFile "meta.json"))) 
              $(embedFile "tantivy_meta.json")

tantivyBuildIndex zettelkasten indexDir = do
  withProcessWait_
    (proc "tantivy" ["index","-i", toFilePath indexDir] 
        |> setStdin createPipe
        |> setStdout byteStringOutput)
    (\p -> do
      let handle = getStdin p
      listZettels zettelkasten >>= traverse_ (\lnk -> do 
        zettel <- loadZettel zettelkasten (linkTarget lnk)
        LT.hPutStrLn handle (exportAsTantifyJSON zettel)
        )
      hClose handle
    )

tantivySearch indexDir queryText = do
    stdout <- readProcessStdout_ 
      (proc "tantivy"
        ["search"
        , "-i"
        , toFilePath indexDir
        , "-q", toString queryText]
      )
    Char8.lines stdout |> concatMap decode |> pure 
 where
   decode :: Char8.ByteString -> [Link]
   decode line = case Aeson.eitherDecode line of
        Left err -> error ("Tantify search output parsing failed: "<> toText err) -- TODO, error
        Right (TantivyOutput txts) -> map (\t -> Link t Nothing Nothing) txts

newtype TantivyOutput = TantivyOutput {identifier :: [Text]}
    deriving (Eq,Show,Generic,Aeson.FromJSON)

--Work for Find linkless zettels and other structural things.

-- Find the 'neighbourhood' of the zettel. 
neighbourhoodAndLinks :: [Link] -> LinkStructure -> Text -> HashSet Text
neighbourhoodAndLinks links ls@(LS thisLinksTo thisHasLinkFrom _) zettelName 
    = let
        fromHere = mLookup zettelName thisLinksTo  
        (parents,siblings) =  neighbourhood links ls zettelName
      in fromHere <> siblings <> parents

-- Find origin tree of zettel
originTree :: LinkStructure -> Text -> (Tree Text)
originTree = originTree' mempty 
originTree' visited linkStructure zettelName 
    = let
       backlinks = [ backlinker :: Text
                   | backlinker <- HashSet.toList 
                                   (mLookup zettelName (hasLinkFrom linkStructure))
                   , "Origin" `HashSet.member`
                         (mLookup
                             (backlinker,zettelName)
                             (linkLabel linkStructure))
                   , not (backlinker `HashSet.member` visited)
                   ]
       visitedNow = visited <> HashSet.fromList backlinks 
      in Node zettelName (map (originTree' visitedNow linkStructure) backlinks)


-- Find the origin chain of a zettel
originChain :: ZettelKasten -> Text -> IO [Link]
originChain zettelkasten zettelName = 
        let loop acc zettel = do
                z <- loadZettel zettelkasten zettel
                case findOriginLink (namedValue z) of
                    Nothing -> pure acc
                    Just origin     
                        -- Let's not loop if there is a cycle:
                        | (origin) `elem` acc -> pure acc
                        | otherwise         -> loop (origin:acc) (linkTarget origin)
        in loop [] zettelName >>= reverse .> pure 

mLookup key hashmap = HashMap.lookup key hashmap |> fromMaybe mempty 

neighbourhood :: [Link] -> LinkStructure -> Text -> (HashSet Text, HashSet Text)
neighbourhood links (LS thisLinksTo thisHasLinkFrom _) zettelName =
    let 
      linkedFrom = fromMaybe mempty (HashMap.lookup zettelName thisHasLinkFrom)
      siblings   = flip foldMap linkedFrom $ \linkingZettel ->
                     mLookup linkingZettel thisLinksTo 
    in (linkedFrom,siblings)

fillMissingLinks zettelkasten zettel =
  let labels = getPotentialLabels zettel
  in  if null labels
        then pure zettel
        else case findOriginLink (namedValue zettel) of
          Nothing     -> pure zettel
          Just origin -> do
            originZettel <- loadZettel zettelkasten (linkTarget origin)
            let lnks =
                  [ lnk
                  | lnk <- links (namedValue originZettel)
                  , ref <- maybeToList (refNo lnk)
                  , ref `elem` labels
                  ]
            let originRefs =
                  [ bib
                  | bib <- references (namedValue originZettel)
                  , bibKey bib `elem` labels
                  ]
            pure (fmap (addLinks lnks .> addReferences originRefs) zettel)


-- Store metadata: TODO: Split this off

setAutoStoreLinkage :: Path Abs File -> ZettelKasten -> IO ZettelKasten
setAutoStoreLinkage pth zettelkasten = do
    conn <- createLinkageDB pth
    let saveZettelWithLinks nzettel = do
          refreshLinks conn nzettel
          saveZettel zettelkasten nzettel
    pure (zettelkasten{saveZettel=saveZettelWithLinks})

    
createElucidationDB :: Path Abs File -> IO SQL.Connection
createElucidationDB dbPath = do
   conn <- SQL.open (toFilePath dbPath)
   SQL.execute_ conn "CREATE TABLE IF NOT EXISTS elucidations\
                 \(hash BLOB PRIMARY KEY, weight REAL, timestamp TEXT)"
   pure conn 

decreaseElucidationPs :: SQL.Connection -> Double -> [(Double,ByteString)] -> IO ()
decreaseElucidationPs conn decr hashes
 = SQL.withTransaction conn <| for_ hashes <| \(weight,hash) -> do
    now <- getCurrentTime
    SQL.execute conn "INSERT OR IGNORE INTO elucidations(hash,weight,timestamp) VALUES (?,?,?)"
        (hash,weight,now)
    SQL.execute conn "UPDATE elucidations SET weight=weight*? WHERE hash=?"
        (decr,hash)

getStoredWeight :: SQL.Connection -> Double -> ByteString -> IO Double
getStoredWeight conn def hash = do
    xs <- SQL.query conn "SELECT weight FROM elucidations WHERE hash=?"
        (SQL.Only hash)
    case xs of
        []     -> pure def
        values -> values |> map SQL.fromOnly |> F.maximum |> pure

-- autoSearchOnTitle :: ZettelKasten -> Path Abs Dir -> Text -> IO [Link]
--autoSearchOnTitle zettelkasten tantivyIndex zettelName 
-- = doTantivySearch zettelkasten 
--                   tantivyIndex 
--                   RebuildIndex -- Be smart with this
--                   (guessTantivySearch zettelName)
--
---- TODO: This violates the do-not-process-text principle
--guessTantivySearch :: Text -> Text
--guessTantivySearch zettelTitle = 
--    let 
--     noUID = T.drop (T.length "2135546E-230E-40D3-91ED-D40D87F77205-") zettelTitle
--     query = T.map (\x -> if Data.Char.isAlpha x then x else ' ') noUID |> words  |> unwords
--    in query



