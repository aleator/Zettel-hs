{-#LANGUAGE TupleSections#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE GADTs#-}
module Elucidations where

import qualified Data.Text                     as T
import qualified Data.HashSet                  as HashSet

import Zettel
import Operations
import ZettelKasten

import Data.Proxy

import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict (HashMap)

import Crypto.Hash.SHA256
import qualified Data.Text.Encoding                     as T

data Elucidation 
    = IsUnlinked [Text] 
    | CouldBeTooLong [Text]
    deriving (Eq,Ord,Show)

coalesce :: [Elucidation] -> [Elucidation]
coalesce [] = []
coalesce (IsUnlinked t : IsUnlinked g : xs) =
  coalesce (IsUnlinked (t ++ g) : xs)
coalesce (CouldBeTooLong t : CouldBeTooLong g : xs) =
  coalesce (CouldBeTooLong (t ++ g) : xs)
coalesce (x : y : xs) = x : coalesce (y : xs)
coalesce [x         ] = [x]

hashElucidation elucidation 
   = case elucidation of
      IsUnlinked l     -> hash (T.encodeUtf8 (T.concat ("A":l)))
      CouldBeTooLong l -> hash (T.encodeUtf8 (T.concat ("B":l)))

prettyPrint :: [Elucidation] -> Text
prettyPrint = sort .> coalesce .> map pp .> intersperse "" .> unlines
 where
  pp (CouldBeTooLong ts) 
    = "These notes might be better split into\
     \ smaller notes:\n"<>T.unlines ts
  pp (IsUnlinked ts)  =
            "Can you link to these note from somehere?\n"
             <> T.unlines ts



-- The main function
elucidate :: ZettelKasten -> IO [(Double,Elucidation)]
elucidate zettelkasten = do
    (linkStructure,longOnes) <-
        scanZettelKasten zettelkasten
            (\z ->(linkExtraction z,longScanner z))
    
    let unlinkedZettels = unlinkScanner linkStructure
    
    let weighted n x = map (\i -> (x,n [i]))

    pure <|
        (longOnes :: [(Double,Elucidation)])
         <> 
        (unlinkedZettels)


simpleLengthMeasure zettel 
  = let
      bodyLines = body zettel |> lines |> length
      linkCount = links zettel |> length
    in fromIntegral (bodyLines - linkCount)

unlinkScanner :: LinkStructure -> [(Double,Elucidation)]
unlinkScanner (LS thisLinksTo thisHasLinkFrom _) =
  foldMap 
    (\zettelLink -> case HashMap.lookup zettelLink thisHasLinkFrom of
                      Nothing -> [(max 0 (1-outgoing),IsUnlinked [zettelLink])]
                         where outgoing = HashMap.lookup zettelLink thisLinksTo
                                        |> length |> fromIntegral |> (* 0.1)
                      Just _ -> mempty
    )
    (HashMap.keys thisLinksTo) -- (map linkTarget links)

longScanner :: Named Zettel -> [(Double,Elucidation)]
longScanner zettel 
    | measure > 0
      = [(weight,CouldBeTooLong [ name zettel ])]
    | otherwise 
      = mempty
 where 
  weight = 1 - (0.5/(1+measure*0.2))
  measure =   simpleLengthMeasure (namedValue zettel) - 60 



