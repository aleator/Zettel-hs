{-#language DeriveFunctor#-}
{-#language DeriveGeneric#-}
{-#language TemplateHaskell#-}
{-#language DeriveAnyClass#-}
module Operations where

import Zettel
import Parser
import Data.UUID
import Data.UUID.V4
import Data.Char
import Data.Time.Clock 
import Data.Time.LocalTime 
import Data.Time.Calendar 
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson as Aeson
import Path
import System.Random
import Control.Exception (assert)

linkTo :: Named a -> Link
linkTo named = Link (name named) Nothing Nothing

maulToFilename :: Text -> Path Rel File
maulToFilename text = 
   let  noSpace x 
            | isSpace x = '-'
            | x == '/'  = '_'
            | not (isAlphaNum x || '-' == x) = '_'
            | otherwise = toLower x
   in case parseRelFile (Prelude.toString (T.map noSpace text)) of 
                   Right aPath 
                    | parent aPath == $(mkRelDir ".")
                        -> aPath
                    | otherwise -> error ("Cannot create a title that isn't a valid filepath")    
                   Left e -> error (show e) -- TODO Exception!

junkAlphabet :: [Char]
junkAlphabet =
    "0123456789" -- ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     ++ [chr n | n <- [0x2190..0x2199] ++ [0x1900 ..0x191E] ++ [0x0F50 .. 0x0F6C] ] 

junkAlphabetLength :: Int
junkAlphabetLength = length junkAlphabet

-- TODO: Move somewhere:
check :: a -> (a -> Bool) -> a
x `check` p = assert (p x) x

mkName :: Text -> IO Text
mkName title = do
   let titleFN = maulToFilename title `check` (toFilePath .> Prelude.null .> not)
   now <- getCurrentTime 
   zone <- getCurrentTimeZone
   let (year,month,day) = utctDay now |> toGregorian 
   let generate state n 
        | n <= 0 = pure []
        | otherwise = do
            skips <- randomRIO (0,junkAlphabetLength)
            let (char:stateNext) = drop skips state
            rest <- generate stateNext (n-1)
            pure (char:rest)
   let 
    LocalTime _ localTimeOfDay = utcToLocalTime zone now
    between a b = todHour localTimeOfDay >= a && todHour localTimeOfDay < b 
    timeOfDay 
        | between 0 6   = "night" 
        | between 6 11  = "morning"
        | between 11 13 = "midday"
        | between 13 18 = "afternoon"
        | between 18 24 = "evening"
        | otherwise     = "outside_time"
   junk <- generate (cycle junkAlphabet) (2::Int)
   let uuid =   show year  <>"-"
             <> show month <>"-"
             <> show day   <>"-"
             <> timeOfDay   <>"-"
             <> "⟦" <> Prelude.toText junk   <>"⟧-"
   pure (uuid <> Prelude.toText (toFilePath titleFN))


mkNameOld :: Text -> IO Text
mkNameOld title = do
   let titleFN = maulToFilename title
   uuid <- nextRandom >>= Data.UUID.toString .> map toUpper .> Prelude.toText .> pure
   pure (uuid<>"-"<> Prelude.toText (toFilePath titleFN))

create :: Text -> IO (Named Zettel)
create title = do
   name <- mkName title
   pure (Named name (Zettel title mempty mempty mempty mempty))

addRefId :: Text -> Link -> Link
addRefId newRefId (Link name rel _oldRefId) = Link name rel (Just newRefId)

addLinks :: [Link] -> Zettel -> Zettel
addLinks lnks zettel = zettel { links = ordNub (links zettel ++ lnks) }

addReferences :: [BibItem] -> Zettel -> Zettel
addReferences refs zettel =
  zettel { references = ordNub (references zettel ++ refs) }

createLinked
  :: Named Zettel
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> IO (Named Zettel, Named Zettel)
createLinked (Named start zettel) refID relation newTitle = do
  newName <- mkName newTitle
  let zettelNew = Zettel newTitle
                         mempty
                         mempty
                         mempty
                         [Link start (Just "Origin") (Just "Origin")]
  let zettelUpdated = addLinks [Link newName relation refID] zettel
  pure (Named start zettelUpdated, Named newName zettelNew)

findOriginLink :: Zettel -> Maybe Link
findOriginLink zettel = find
  (\lnk -> fmap T.strip (description lnk) == Just "Origin" || refNo lnk == Just "Origin")
  (links zettel)

bodyChunks :: Named Zettel -> Either String [Text]
bodyChunks namedZettel 
  = body (namedValue namedZettel) 
    |> textChunks (name namedZettel |> Prelude.toString)

exportAsJSON :: Named Zettel -> LT.Text
exportAsJSON = Aeson.encodeToLazyText

exportAsTantifyJSON :: Named Zettel -> LT.Text
exportAsTantifyJSON (Named name zettel) = 
  Aeson.encodeToLazyText (Aeson.object 
        ["body" Aeson..= (body zettel)
        ,"title" Aeson..= (title zettel)
        ,"identifier" Aeson..= name])


--- Body Parser related
getLabelsWithShortContext :: Named Zettel -> [(Label,Text,Text)]
getLabelsWithShortContext = getLabelsWithContext .> map shorten 
    where
     shorten (label,before,after) = (label, pickLine last before, pickLine head after)
     pickLine n = lines .> filter (/="") .> nonEmpty .> fmap n .> fromMaybe ""
    

getLabelsWithContext :: Named Zettel -> [(Label,Text,Text)]
getLabelsWithContext z = case runTheParser (name z |> Prelude.toString)   
                                           (namedValue z |> body)
                                           labelSoup of
                        Left _err -> []
                        Right v -> getLabelContexts v

getPotentialLabels :: Named Zettel -> [Label]
getPotentialLabels z = case runTheParser (name z |> Prelude.toString)   
                                         (namedValue z |> body)
                                         labelSoup of
                        Left _err -> []
                        Right v -> rights v

newtype Placeholder = Placeholder Text
replacePlaceholder :: Placeholder -> Text -> Zettel -> Zettel
replacePlaceholder (Placeholder placeholder) text zettel
    = zettel{ body = body zettel |> f }
    where
     f bodyText = T.replace placeholder text bodyText
    
