{-#language DeriveFunctor#-}
{-#language DeriveGeneric#-}
{-#language TemplateHaskell#-}
{-#language DeriveAnyClass#-}
module Operations where

import Parser -- TODO Extract type
import Data.UUID
import Data.UUID.V4
import Data.Char
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson as Aeson
import Path


data Named a = Named {name :: Text, namedValue :: a}
 deriving (Show,Functor,Generic,Aeson.ToJSON,Aeson.FromJSON)

linkTo named = Link (name named) Nothing Nothing

mkName :: Text -> IO Text
mkName title = do
   let  noSpace x 
            | isSpace x = '-'
            | x == '/'  = '#'
            | not (isAlphaNum x || '-' == x) = '#'
            | otherwise = toLower x
        titleFN = case parseRelFile (Prelude.toString (T.map noSpace title)) of 
                   Right aPath 
                    | parent aPath == $(mkRelDir ".")
                        -> aPath
                    | otherwise -> error ("Cannot create a title that isn't a valid filepath")    
                   Left e -> error (show e) -- TODO Exception!


   uuid <- nextRandom >>= Data.UUID.toString .> map toUpper .> Prelude.toText .> pure
   pure (uuid<>"-"<> Prelude.toText (toFilePath titleFN))
-- TODO: The above does all kinds of nasty with evil file paths

create :: Text -> IO (Named Zettel)
create title = do
   name <- mkName title
   pure (Named name (Zettel title mempty mempty mempty mempty))

addRefId :: Text -> Link -> Link
addRefId newRefId (Link name rel _oldRefId) = Link name rel (Just newRefId)

addLinks lnks zettel = zettel{links = ordNub (links zettel++lnks)}

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
