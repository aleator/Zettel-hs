{-#LANGUAGE OverloadedStrings#-}
module Visualization where

import Data.Tree

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
                    
