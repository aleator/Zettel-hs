{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveAnyClass#-}
{-#OPTIONS_GHC -fno-warn-unused-do-bind#-}
module Parser where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import qualified Data.Char                     as Char

import           System.Directory

import           Zettel

parseSeparator :: Parsec Void Text ()
parseSeparator = try ((separatorLine <?> "Ascii section separator") $> ())
  <|> try ((unicodeSeparatorLine <?> "Unicode section separator") $> ())

isReferenceLine, isSeparatorLine :: (Eq a, IsString a) => a -> Bool
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

nonLinebreakingSpace :: Char -> Bool
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


labelSoup :: Parsec Void Text [Either Text Label]
labelSoup = do
    start <- takeWhileP Nothing (/='[')
    labelName <- try $ optional $ do
                         (Right <$> refLabel) <|> ("["*>pure (Left "["))
    rest <- if T.null start && isNothing labelName
                then pure []
                else labelSoup
    pure <| maybe (Left start:rest) (\x -> Left start:x:rest) labelName


parseLinks :: ParsecT Void Text Identity [Link]
parseLinks = 
  "Links:" *> linespace *> newline *> Text.Megaparsec.many (link)

emptyLine :: ParsecT Void Text Identity ()
emptyLine = try (linespace <* newline)

link :: ParsecT Void Text Identity Link
link = do
  ref  <- optional refId
  linkName <- try (linkWord "Link")

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
  pure (Link linkName (fmap T.strip desc) ref)

singleLineRef :: ParsecT Void Text Identity BibItem
singleLineRef = do
    ri <- refId
    line1 <- parseLine
    pure (BibItem ri line1)

multilineRef = do
    ri <- refId
    line1 <- parseLine
    refLines <- Text.Megaparsec.many parseIndentedLine
    skipMany emptyLine
    pure (BibItem ri (unlines (line1:refLines)))

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
  titleText <- (parseLine <?> "titleLine") <* parseSeparator <* newline
  skipMany emptyLine
  zettelBody <- optional 
    (unlines <$> Text.Megaparsec.some (try parseLine <|> (newline $> "")))

  -- references
  refs <- optional <| do
    try (referenceLine <|> unicodeReferenceLine) <* newline
    skipMany emptyLine
    Text.Megaparsec.many multilineRef
  
  -- meta
  parseSeparator<*newline
  zettelTags <- parseTags
  newline
  zettelLinks <- parseLinks
  skipMany emptyLine
  parseSeparator

  space
  takeRest
  pure (Zettel titleText (fromMaybe "" zettelBody) (fromMaybe [] refs) zettelTags zettelLinks)

                
tst :: IO ()
tst = do
  files' <- listDirectory "/Users/aleator/zettel/"
  let files = filter (not . ("." `isPrefixOf`)) files'
  setCurrentDirectory "/Users/aleator/zettel/"
  ts <- traverse readFileText files
  mapM_
    (\(n, x) -> case parse zettel n x of
      Left  e -> putStrLn (errorBundlePretty  e)
      Right _ -> --putTextLn (pprZettel v)
                 pass
    )
    (zip files ts)
