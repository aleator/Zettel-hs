{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE OverloadedStrings#-}
module Parser where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Void
import qualified Data.Char                     as Char

import           System.Directory

data Link = Link {linkTarget::Text
                 ,description :: Maybe Text
                 ,refNo :: Maybe Text}
    deriving (Generic,Show,Eq,Ord)

data Zettel = Zettel {title :: Text
                     ,body :: Text
                     ,tags :: [Text]
                     ,links :: [Link]}
    deriving (Generic,Show)

parseSeparator :: Parsec Void Text ()
parseSeparator = separatorLine $> ()

separatorLine :: IsString s => s
separatorLine =
  "--------------------------------------------------------------------------------"

parseLine :: Parsec Void Text Text
parseLine = do
  ln <- takeWhile1P Nothing (/= '\n')
  optional newline
  when (ln == separatorLine) (fail "Not a line (found  separator)")
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
                   (\c -> Char.isAlphaNum c || nonLinebreakingSpace c) --TODO
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


runZettelParser :: FilePath -> Text -> Either String Zettel
runZettelParser originFile input =
    first errorBundlePretty (parse zettel originFile input)

zettel :: Parsec Void Text Zettel
zettel = do
  title <- parseLine <* parseSeparator <* newline
  skipMany emptyLine
  body <- optional 
    (unlines <$> Text.Megaparsec.some (try parseLine <|> (newline $> "")))
  parseSeparator<*newline
  tags <- parseTags
  newline
  links <- parseLinks
  skipMany emptyLine
  parseSeparator
  space
  takeRest
  pure (Zettel title (fromMaybe "" body) tags links)

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

tst = do
  files' <- listDirectory "/Users/aleator/zettel/"
  let files = filter (not . ("." `isPrefixOf`)) files'
  setCurrentDirectory "/Users/aleator/zettel/"
  ts <- traverse readFileText files
  mapM_
    (\(n, x) -> case parse zettel n x of
      Left  e -> putStrLn (errorBundlePretty  e)
      Right v -> putTextLn (pprZettel v)
                 --pass
    )
    (zip files ts)
