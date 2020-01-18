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
parseSeparator = separatorLine $> ()

separatorLine :: IsString s => s
separatorLine =
  "--------------------------------------------------------------------------------"

referenceLine :: IsString s => s
referenceLine =
  "----- External references ------------------------------------------------------"

parseIndentedLine :: Parsec Void Text Text
parseIndentedLine = do
  spaces <- takeWhile1P (Just "Indent") nonLinebreakingSpace
  ln <- takeWhile1P Nothing (/= '\n')
  optional newline
  when (ln == separatorLine) (fail "Not a line (found separator)")
  when (ln == referenceLine) (fail "Not a line (found reference separator)")
  pure (spaces<>ln)

parseLine :: Parsec Void Text Text
parseLine = do
  ln <- takeWhile1P Nothing (/= '\n')
  optional newline
  when (ln == separatorLine) (fail "Not a line (found separator)")
  when (ln == referenceLine) (fail "Not a line (found reference separator)")
  pure ln

parseTags :: Parsec Void Text [Text]
parseTags = do
  chunk "Tags:"
  satisfy (`elem` [' ', '\t']) 
  let tag = takeWhile1P (Just "Tag") (not . (`elem` [',', '\n'])) --TODO
  sepBy (tag <* linespace) ("," <* linespace) -- <* newline

linespace :: Parsec Void Text ()
linespace = skipMany (satisfy nonLinebreakingSpace)

nonLinebreakingSpace c = Char.isSpace c && c /= '\n' -- TODO: Other linebreaks?

word :: String -> Parsec Void Text Text
word n = do
  w <- takeWhile1P (Just n) (not . (`elem` [' ', ',', '\t', '\n'])) --TODO
  when (w == separatorLine) (fail "Unexpected Separator")
  pure w

refId :: Parsec Void Text Text
refId = do
  "["
  w <- takeWhile1P (Just "ref-id") 
                   (\c -> Char.isAlphaNum c || nonLinebreakingSpace c || elem c ("#-"::[Char])) --TODO
  "]:"
  linespace
  pure w

parseLinks = 
  "Links:" *> linespace *> newline *> Text.Megaparsec.many (link <* newline)

emptyLine = linespace <* newline

link = do 
    ref <- optional refId
    link <- try (word "Link")
    desc <- optional (takeWhile1P (Just "Link description") (/= '\n'))
    pure (Link link desc ref)

multilineRef = do
    ri <- refId
    line1 <- parseLine
    lines <- Text.Megaparsec.many parseIndentedLine
    skipMany emptyLine
    pure (BibItem ri (unlines (line1:lines)))

runZettelParser :: FilePath -> Text -> Either String Zettel
runZettelParser originFile input =
    first errorBundlePretty (parse zettel originFile input)

zettel :: Parsec Void Text Zettel
zettel = do
  title <- parseLine <* parseSeparator <* newline
  skipMany emptyLine
  body <- optional 
    (unlines <$> Text.Megaparsec.some (try parseLine <|> (newline $> "")))

  -- references
  refs <- optional <| do
    referenceLine <* newline
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
    <> separatorLine
    <> "\n"
    <> "\n"
    <> T.stripEnd (body zettel)
    <> "\n"
    <> "\n"
    <> referenceLine <> "\n\n"
    <> pprRefs (references zettel)
    <> separatorLine
    <> "\n"
    <> "Tags: "
    <> T.intercalate ", " (tags zettel)
    <> "\n"
    <> "Links: "
    <> "\n"
    <> unlines
         [  maybe "" (\ref -> "["<>ref<>"]: ") ref
            <> maybe lnk (\d -> lnk <> " " <> d) desc
         | Link lnk desc ref <- links zettel
         ]
    <> separatorLine
   where 
     pprRefs = map pprBib .> unlines
     pprBib (BibItem ref txt) = "["<>ref<>"]: "<>txt
                
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
