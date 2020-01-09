{-#language DeriveFunctor#-}
{-#language DeriveGeneric#-}
{-#language DeriveAnyClass#-}
module Operations where

import Parser -- TODO Extract type
import Data.UUID
import Data.UUID.V4
import Data.Char
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson as Aeson


data Named a = Named {name :: Text, namedValue :: a}
 deriving (Show,Functor,Generic,Aeson.ToJSON,Aeson.FromJSON)

linkTo named = Link (name named) Nothing Nothing

mkName :: Text -> IO Text
mkName title = do
   uuid <- nextRandom >>= Data.UUID.toString .> map toUpper .> Prelude.toText .> pure
   pure (uuid<>"-"<>title)

create :: Text -> IO (Named Zettel)
create title = do
   name <- mkName title
   pure (Named name (Zettel title mempty mempty mempty))

addLinks lnks zettel = zettel{links = ordNub (links zettel++lnks)}

createLinked (Named start zettel) relation newTitle = do
    newName <- mkName newTitle
    let zettelNew = Zettel newTitle mempty mempty [ Link start (Just "Origin") Nothing ]
    let zettelUpdated = addLinks [Link newName relation Nothing] zettel
    pure (Named start zettelUpdated, Named newName zettelNew)

exportAsJSON :: Named Zettel -> LT.Text
exportAsJSON = Aeson.encodeToLazyText

exportAsTantifyJSON :: Named Zettel -> LT.Text
exportAsTantifyJSON (Named name zettel) = 
  Aeson.encodeToLazyText (Aeson.object 
        ["body" Aeson..= (body zettel)
        ,"title" Aeson..= (title zettel)
        ,"identifier" Aeson..= name])
