module Graphviz
( node_to_dot
) where

import Parser
import UtilString

node_to_dot :: Node -> String
node_to_dot node = case node of
    Program cs -> map
        (\n -> "Program -> ")
    Container (begin,end) cs -> _
    Literal str
    Special str
    _ -> ""