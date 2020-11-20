{-#LANGUAGE OverloadedStrings#-}
module Visualization where

import Data.Tree
import qualified Data.Text as T

drawBacklinker :: Text -> [(Text,Text,Text)] -> Text
drawBacklinker from links = 
    let 
      header = padWithTo "─" 80 ("┌ " <> from <> " ") 
      footer =  padWithTo "─" 80 "└"
      content =
                concatMap (drawBacklink from) links |>
                mapHeadTail ("├─"<>) ("├─"<>) ("│ "<>)
    in unlines (header : content ++ [footer])

mapHeadTail :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail onHead onTail onBody list 
    = case list of
        [] -> []
        (x:xs) -> onHead x : mapTail xs
    where 
      mapTail [] = []
      mapTail [x] = [onTail x]
      mapTail (x:xs) = onBody x : mapTail xs
       


drawBacklink :: Text -> (Text,Text,Text) -> [Text]
drawBacklink linkerName (label, before, after) = 
    let
      header = padWithTo "─" 78 ("" ) 
      footer =  padWithTo "─" 78 ""
      content = lines (before<>" "<>label<>" "<>after)
    in (header : content ++ [footer])


padWithTo c n x = x <>T.replicate (n-T.length x) c

drawUnicode :: Tree Text -> [Text]
drawUnicode (Node v nodes) = v:loop nodes
 where
   loop []     = [] 
   loop [x]    = case drawUnicode x of
                    (fstLine:rest) -> " └─ " <> fstLine : map ("   "<>) rest
                    [] -> []
   loop (x:xs) = case drawUnicode x of
                    [] -> []
                    (start:rest) -> " ├─ "<>start
                                        : map (" │ "<> ) rest++loop xs

testTreeLinear :: Tree Text
testTreeLinear = Node "A" 
                   [Node "B" [Node "C" [Node "D" []]]] 
testBranch :: Tree Text
testBranch = Node "Topic" 
                   [Node "A" []
                   ,Node "B" []
                   ,Node "C" []
                   ,Node "D" []]

testTree :: Tree Text
testTree = Node "A" 
            [Node "Bar" []
            ,Node "Baz" [
                 Node "Guz1" []
                ,Node "Guz2" [
                    Node "A" [], Node "B" [], Node "C" 
                            [Node "Q" [], Node "Bree" [], Node "Cee" []]
                    
                    ]
                ,Node "Guz3" []
                ]
            ,Node "Zook" []
            ] 
                    
