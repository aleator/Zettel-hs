{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveAnyClass#-}
module Parser where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Void
import qualified Data.Char                     as Char
import qualified Data.Aeson as Aeson

import           System.Directory

data Link = Link {linkTarget::Text
                 ,description :: Maybe Text
                 ,refNo :: Maybe Text}
    deriving (Generic,Show,Eq,Ord,Aeson.ToJSON,Aeson.FromJSON)

data BibItem = BibItem {bibKey :: Text, bibText :: Text}
    deriving (Generic,Show,Eq,Ord,Aeson.ToJSON,Aeson.FromJSON)

data Zettel = Zettel {title :: Text
                     ,body :: Text
                     ,references :: [BibItem]
                     ,tags :: [Text]
                     ,links :: [Link]}
    deriving (Generic,Show,Aeson.ToJSON,Aeson.FromJSON)

parseSeparator :: Parsec Void Text ()
parseSeparator = try ((separatorLine <?> "Ascii section separator") $> ())
  <|> try ((unicodeSeparatorLine <?> "Unicode section separator") $> ())

separatorLine :: IsString s => s
separatorLine =
  "--------------------------------------------------------------------------------"
unicodeSeparatorLine :: IsString s => s
unicodeSeparatorLine =
  "────────────────────────────────────────────────────────────────────────────────"

referenceLine :: IsString s => s
referenceLine =
  "----- External references ------------------------------------------------------"

unicodeReferenceLine :: IsString s => s
unicodeReferenceLine =
  "───── External references ──────────────────────────────────────────────────────"

isReferenceLine x = x == unicodeReferenceLine || x == referenceLine
isSeparatorLine x = x == unicodeSeparatorLine || x == separatorLine


parseIndentedLine :: Parsec Void Text Text
parseIndentedLine = do
  spaces <- takeWhile1P (Just "Indent") nonLinebreakingSpace
  ln <- parseLine
  pure (spaces<>ln)

parseNonEmptyLine :: Parsec Void Text Text
parseNonEmptyLine = try $ do
    line <- parseLine
    when (T.all Char.isSpace line) (fail "Expected non empty line")
    pure line

parseLine :: Parsec Void Text Text
parseLine = do
  ln <- takeWhile1P Nothing (/= '\n')
  optional newline
  when (isSeparatorLine ln) (fail "Not a line (found separator)")
  when (isReferenceLine ln) (fail "Not a line (found reference separator)")
  pure ln

parseTags :: Parsec Void Text [Text]
parseTags = do
  chunk "Tags:"
  (do 
     satisfy (`elem` [' ', '\t']) 
     let tag = takeWhile1P (Just "Tag") (not . (`elem` [',', '\n'])) --TODO
     sepBy (tag <* linespace) ("," <* linespace)
   ) <|> pure []

linespace :: Parsec Void Text ()
linespace = skipMany (satisfy nonLinebreakingSpace)

nonLinebreakingSpace c = Char.isSpace c && c /= '\n' -- TODO: Other linebreaks?

word :: String -> Parsec Void Text Text
word n = do
  w <- takeWhile1P (Just n) (not . (`elem` [' ', ',', '\t', '\n'])) --TODO
  when (isSeparatorLine w) (fail "Unexpected Separator")
  pure w

linkWord :: String -> Parsec Void Text Text
linkWord n = do
  w <- takeWhile1P (Just n) (not . (`elem` [' ', '\t', '\n'])) --TODO
  when (isSeparatorLine w) (fail "Unexpected Separator")
  pure w

refLabel :: Parsec Void Text Text
refLabel = try <| do
  "["
  w <- takeWhile1P
    (Just "ref-id")
    (\c ->
      Char.isAlphaNum c 
      || nonLinebreakingSpace c 
      || elem c ("#-,'\"?!:;<>." :: [Char])
    ) --TODO
  "]"
  pure w

refId :: Parsec Void Text Text
refId = try <| do
  w <- refLabel <* ":"
  linespace
  pure w

type Label = Text

labelSoup :: Parsec Void Text [Either Text Label]
labelSoup = do
    start <- takeWhileP Nothing (/='[')
    label <- try $ optional $ do
                         (Right <$> refLabel) <|> ("["*>pure (Left "["))
    rest <- if T.null start && isNothing label 
                then pure []
                else labelSoup
    pure <| maybe (Left start:rest) (\x -> Left start:x:rest) label --pure (Left start:label:rest)


parseLinks = 
  "Links:" *> linespace *> newline *> Text.Megaparsec.many (link)

emptyLine = try (linespace <* newline)

link = do
  ref  <- optional refId
  link <- try (linkWord "Link")

  desc <- do
    onSameLine       <- (takeWhileP (Just "Link description") (/= '\n'))
    mAdditionalLines <- optional <| do
      newline
      restOfLines <- (Text.Megaparsec.many parseIndentedLine)
      skipMany emptyLine
      pure restOfLines
    case mAdditionalLines of
        Nothing | T.null onSameLine -> pure Nothing
        Nothing                     -> pure (Just onSameLine)
        Just thelines               -> unlines (onSameLine : thelines) |> Just |> pure

  Text.Megaparsec.many newline
  pure (Link link (fmap T.strip desc) ref)

singleLineRef = do
    ri <- refId
    line1 <- parseLine
    pure (BibItem ri line1)

multilineRef = do
    ri <- refId
    line1 <- parseLine
    lines <- Text.Megaparsec.many parseIndentedLine
    skipMany emptyLine
    pure (BibItem ri (unlines (line1:lines)))

runTheParser :: FilePath -> Text -> Parsec Void Text a 
                -> Either String a
runTheParser originFile input parser
    = first errorBundlePretty (parse parser originFile input)

runSingleLineBibParser :: FilePath -> Text -> Either String BibItem
runSingleLineBibParser originFile input
    = first errorBundlePretty (parse singleLineRef originFile input)

runZettelParser :: FilePath -> Text -> Either String Zettel
runZettelParser originFile input =
    first errorBundlePretty (parse zettel originFile input)

textChunks :: FilePath -> Text -> Either String [Text]
textChunks originFile input 
   = parse chunker originFile input |> first errorBundlePretty 
  where
   chunker = 
        sepBy (T.unlines <$> Text.Megaparsec.some parseNonEmptyLine) 
              (Text.Megaparsec.some emptyLine)


zettel :: Parsec Void Text Zettel
zettel = do
  title <- (parseLine <?> "titleLine") <* parseSeparator <* newline
  skipMany emptyLine
  body <- optional 
    (unlines <$> Text.Megaparsec.some (try parseLine <|> (newline $> "")))

  -- references
  refs <- optional <| do
    try (referenceLine <|> unicodeReferenceLine) <* newline
    skipMany emptyLine
    Text.Megaparsec.many multilineRef
  
  -- meta
  parseSeparator<*newline
  tags <- parseTags
  newline
  links <- parseLinks
  skipMany emptyLine
  parseSeparator

  space
  takeRest
  pure (Zettel title (fromMaybe "" body) (fromMaybe [] refs) tags links)

pprZettel :: Zettel -> Text
pprZettel zettel =
  title zettel
    <> "\n"
    <> unicodeSeparatorLine
    <> "\n"
    <> "\n"
    <> T.stripEnd (body zettel)
    <> "\n"
    <> "\n"
    <> unicodeReferenceLine <> "\n\n"
    <> pprRefs (references zettel)
    <> unicodeSeparatorLine
    <> "\n"
    <> "Tags: "
    <> T.intercalate ", " (tags zettel)
    <> "\n"
    <> "Links: "
    <> "\n"
    <> unlines
         [  maybe "" (\ref -> "["<>ref<>"]: ") ref
            <> maybe lnk (\d -> lnk <> " " <> d) desc <> "\n"
         | Link lnk desc ref <- links zettel
         ]
    <> unicodeSeparatorLine
   where 
     pprRefs = map pprBib .> unlines

pprBib :: BibItem -> Text
pprBib (BibItem ref txt) = "["<>ref<>"]: "<>T.strip txt
                
tst = do
  files' <- listDirectory "/Users/aleator/zettel/"
  let files = filter (not . ("." `isPrefixOf`)) files'
  setCurrentDirectory "/Users/aleator/zettel/"
  ts <- traverse readFileText files
  mapM_
    (\(n, x) -> case parse zettel n x of
      Left  e -> putStrLn (errorBundlePretty  e)
      Right v -> --putTextLn (pprZettel v)
                 pass
    )
    (zip files ts)
