module Parser
( parse
) where

import UtilString

-- +============================================
-- | Parsing
-- |
parse :: String -> Node
parse input = head . fst $ helper (Program []) input
    where
        helper :: Node -> String -> ([Node], String)
        helper node ""    = ([node], "")
        helper node input =
            let
                concat_to_children :: [Node] -> Node
                concat_to_children ns = set_children node
                    $ (children node) ++ ns
            in case node of
                --
                ----------------------------------------------------------------
                -- begin program
                Program cs ->
                    --
                    -- start new Container?
                    case extract_next_container input of
                        -- extract next Container
                        Just (node_next_empty, input_next) ->
                            let (nodes_next, input_rest) = helper node_next_empty input_next
                            in helper (concat_to_children nodes_next) input_rest
                        --
                        -- extract next Special?
                        Nothing ->
                            case extract_next_special input of
                                -- extract next Special
                                Just (node_next, input_next) ->
                                    helper (concat_to_children [node_next]) input_next
                                --
                                -- next is Literal?
                                Nothing ->
                                    case extract_next_literal input of
                                        -- extract next Literal
                                        Just (node_next, input_rest) ->
                                            helper (Program $ cs ++ [node_next]) input_rest
                                        --
                                        -- end of input
                                        Nothing -> ([node], "")
                --
                ----------------------------------------------------------------
                -- in a container
                Container (start, end) cs ->
                    --
                    -- at end of current container?
                    -- if input `starts_with` end
                    case at_container_end node input of
                        Just input_rest -> ([node], input_rest)
                        --
                        -- new contents of current container
                        Nothing ->
                            --
                            -- next is start of new Container?
                            case extract_next_container input of
                                -- extract next container
                                Just (node_next_empty, input_next) ->
                                    let (nodes_next, input_rest) = helper node_next_empty input_next
                                    in helper (concat_to_children nodes_next) input_rest
                                --
                                -- next is Special?
                                Nothing ->
                                    case extract_next_special input of
                                        -- extract next Special
                                        Just (node_next, input_rest) ->
                                            helper (concat_to_children [node_next]) input_rest
                                        --
                                        -- next is Literal?
                                        Nothing ->
                                            case extract_next_literal input of
                                                -- extract next Literal
                                                Just (node_next, input_rest) ->
                                                    helper (concat_to_children [node_next]) input_rest
                                                --
                                                -- end of input
                                                Nothing -> ([node], "")
                --
                ----------------------------------------------------------------
                -- just a literal
                Literal string -> ([node], input)


-- +============================================
-- | Nodes
-- |

data Node
    --
    = Program
        { children  :: [Node] }
    --
    | Container
        { brackets  :: (String, String)
        , children  :: [Node] }
    --
    | Special
        { string    :: String }
    --
    | Literal
        { string    :: String }

instance Show Node where
    show node = case node of
        Program cs       -> "\n" `join` (map show cs)
        Container (start, end) cs ->
            start ++ ("," `join` (map show cs)) ++ end
        Special string   -> string
        Literal string   -> case string of
            "\n" -> ""
            _ -> string

set_children :: Node -> [Node] -> Node
set_children node ns = case node of
    Program      _ -> Program ns
    Container bs _ -> Container bs ns
    _              -> node

-- +============================================
-- | Containers
-- |
-- + have a start and end
-- |

containers :: [Node]
containers = map container_empty
    [( "(", ")" )]

extract_next_container :: String -> Maybe (Node, String)
extract_next_container = extract_next_from_selection
    containers at_container_start

at_container_end :: Node -> String -> Maybe String
at_container_end node str = case node of
    Container (start, end) _ ->
        if str `starts_with` end
            then Just $ drop (length end) str
            else Nothing
    _ -> Nothing

at_container_start :: Node -> String -> Maybe String
at_container_start node str = case node of
    Container (start, end) _ ->
        if str `starts_with` start
            then Just $ drop (length start) str
            else Nothing
    _ -> Nothing

container_empty :: (String, String) -> Node
container_empty bs = Container bs []

-- +============================================
-- | Specials
-- |
-- + seperate left from right
-- + cannot be part attached to
-- | any reference word
-- |

specials :: [Node]
specials = map Special
    [ " ", "\n" ]

extract_next_special :: String -> Maybe (Node, String)
extract_next_special = extract_next_from_selection
    specials at_special

at_special :: Node -> String -> Maybe String
at_special node str = case node of
    Special string ->
        if str `starts_with` string
            then Just $ drop (length string) str
            else Nothing
    _ -> Nothing

-- +============================================
-- | Literals
-- |

extract_next_literal :: String -> Maybe (Node, String)
extract_next_literal ""  = Nothing
extract_next_literal str = Just $ helper str ""
    where
        helper :: String -> String -> (Node, String)
        helper output input = case input of
            ""    -> (Literal output, input)
            (c:s) -> case extract_next_special (c:s) of
                Just _  -> (Literal output, input)
                Nothing -> helper (output ++ [c]) s