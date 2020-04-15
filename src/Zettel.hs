{-#language DeriveAnyClass#-}
{-#language DeriveGeneric#-}
{-#language DeriveFunctor#-}
module Zettel where
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

type Label = Text

data Named a = Named {name :: Text, namedValue :: a}
 deriving (Show,Functor,Generic,Aeson.ToJSON,Aeson.FromJSON)

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

