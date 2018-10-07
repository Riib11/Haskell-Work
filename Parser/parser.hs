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

                extract_next_node :: () -> ([Node], String)
                extract_next_node () =  --
                    -- begin new Container?
                    case extract_next_container_begin input of
                        -- extract next Container
                        Just (node_next_empty, input_next) ->
                            let (nodes_next, input_rest) = helper node_next_empty input_next
                            in helper (concat_to_children nodes_next) input_rest
                        --
                        -- next is Seperator
                        Nothing -> case extract_next_seperator input of
                            -- extract next Seperator
                            Just (node_next, input_next) ->
                                helper (concat_to_children [node_next]) input_next
                            --
                            -- next is Special?
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
                                                helper (concat_to_children [node_next]) input_rest
                                            --
                                            -- end of input
                                            Nothing -> ([node], "")

            in case node of
                --
                ----------------------------------------------------------------
                -- begin program
                Program _ -> extract_next_node ()
                --
                ----------------------------------------------------------------
                -- in a container
                Container (begin, end) cs ->
                    case at_container_end node input of
                        -- at end of current container?
                        Just input_rest -> ([node], input_rest)
                        Nothing -> extract_next_node ()
                --
                ----------------------------------------------------------------
                -- leafs
                Seperator _ -> ([node], input)
                Special   _ -> ([node], input)
                Literal   _ -> ([node], input)


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
    | Seperator
        { string    :: String }
    --
    | Special
        { string    :: String }
    --
    | Literal
        { string    :: String }

instance Show Node where
    show node = case node of
        Program cs       -> "\n" `join` (map show cs)
        Container (begin, end) cs ->
            let cs_filtered = filter (\x -> case x of Seperator _ -> False; _ -> True) cs
            in begin ++ (" " `join` (map show cs_filtered)) ++ end
        Seperator string -> ""
        Special string   -> string
        Literal string   -> "" ++ string ++ ""

set_children :: Node -> [Node] -> Node
set_children node ns = case node of
    Program      _ -> Program ns
    Container bs _ -> Container bs ns
    _              -> node

represention_of :: Node -> String
represention_of node = case node of
    Program _ -> "Program"
    Container (b,e) _ -> b ++ " ... " ++ e
    Seperator _ -> "_"
    Special _ -> 

-- +============================================
-- | Containers
-- |
-- + have a begin and end
-- |

containers :: [Node]
containers = map container_empty
    [( "(", ")" )]

extract_next_container_begin :: String -> Maybe (Node, String)
extract_next_container_begin = extract_next_from_selection
    containers at_container_begin

extract_next_container_end :: String -> Maybe (Node, String)
extract_next_container_end = extract_next_from_selection
    containers at_container_end

at_container_end :: Node -> String -> Maybe String
at_container_end node str = case node of
    Container (begin, end) _ ->
        if str `begins_with` end
            then Just $ drop (length end) str
            else Nothing
    _ -> Nothing

at_container_begin :: Node -> String -> Maybe String
at_container_begin node str = case node of
    Container (begin, end) _ ->
        if str `begins_with` begin
            then Just $ drop (length begin) str
            else Nothing
    _ -> Nothing

container_empty :: (String, String) -> Node
container_empty bs = Container bs []

-- +============================================
-- | Seperators
-- |

seperators :: [Node]
seperators = map Seperator
    [ " ", "\n" ]

extract_next_seperator :: String -> Maybe (Node, String)
extract_next_seperator = extract_next_from_selection
    seperators at_seperator

at_seperator :: Node -> String -> Maybe String
at_seperator node = at_next node
    (\n -> case n of
        Seperator string -> Just string
        _ -> Nothing)

-- at_seperator node str = case node of
--     Seperator string ->
--         if str `begins_with` string
--             then Just $ drop (length string) str
--             else Nothing
--     _ -> Nothing

-- +============================================
-- | Specials
-- |
-- + seperate left from right
-- + cannot be part attached to
-- | any reference word
-- |

specials :: [Node]
specials = map Special
    []

extract_next_special :: String -> Maybe (Node, String)
extract_next_special = extract_next_from_selection
    specials at_special

at_special :: Node -> String -> Maybe String
at_special node str = case node of
    Special string ->
        if str `begins_with` string
            then Just $ drop (length string) str
            else Nothing
    _ -> Nothing

-- +============================================
-- | Literals
-- |

extract_next_literal :: String -> Maybe (Node, String)
extract_next_literal ""  = Nothing
extract_next_literal str = Just $ helper "" str
    where
        helper :: String -> String -> (Node, String)
        helper output input = case input of
            ""    -> (Literal output, input)
            (c:s) -> if (any
                (\f -> case f input of Nothing -> False; _ -> True)
                [ extract_next_container_begin
                , extract_next_container_end 
                , extract_next_seperator
                , extract_next_special ])
                then (Literal output, input)
                else helper (output ++ [c]) s
