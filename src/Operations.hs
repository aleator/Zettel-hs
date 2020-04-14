{-#language DeriveFunctor#-}
{-#language DeriveGeneric#-}
{-#language TemplateHaskell#-}
{-#language DeriveAnyClass#-}
module Operations where

import Parser -- TODO Extract type
import Data.UUID
import Data.UUID.V4
import Data.Char
import Data.Time.Clock 
import Data.Time.LocalTime 
import Data.Time.Calendar 
import Data.Text (Text)
import Data.Char
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson as Aeson
import Path
import System.Random


data Named a = Named {name :: Text, namedValue :: a}
 deriving (Show,Functor,Generic,Aeson.ToJSON,Aeson.FromJSON)

linkTo named = Link (name named) Nothing Nothing

maulToFilename title = 
   let  noSpace x 
            | isSpace x = '-'
            | x == '/'  = '_'
            | not (isAlphaNum x || '-' == x) = '_'
            | otherwise = toLower x
  in case parseRelFile (Prelude.toString (T.map noSpace title)) of 
                   Right aPath 
                    | parent aPath == $(mkRelDir ".")
                        -> aPath
                    | otherwise -> error ("Cannot create a title that isn't a valid filepath")    
                   Left e -> error (show e) -- TODO Exception!

junkAlphabet =
    "0123456789" -- ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     ++ [chr n | n <- [0x2190..0x2199] ++ [0x1900 ..0x191E] ++ [0x0F50 .. 0x0F6C] ] -- ++[0x21B0..0x21B7]++[0x27F0..0x27FF]]
junkAlphabetLength = length junkAlphabet

mkName :: Text -> IO Text
mkName title = do
   let titleFN = maulToFilename title
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
   junk <- generate (cycle junkAlphabet) 2
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

addLinks lnks zettel = zettel { links = ordNub (links zettel ++ lnks) }
addReferences refs zettel =
  zettel { references = ordNub (references zettel ++ refs) }

createLinked (Named start zettel) refID relation newTitle = do
    newName <- mkName newTitle
    let zettelNew = Zettel newTitle mempty mempty mempty [ Link start (Just "Origin") (Just "Origin") ]
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

getPotentialLabels :: Named Zettel -> [Label]
getPotentialLabels z = case runTheParser (name z |> Prelude.toString)   
                                         (namedValue z |> body)
                                         labelSoup of
                        Left err -> []
                        Right v -> rights v
    
