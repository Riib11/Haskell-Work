import System.IO

import State

main :: IO ()
main = do loop State.start_state
    where
        loop state = do
            prompt state -- display message and prompt
            input state  -- get user input (parsed to command)
        prompt state = do
            putStrLn ""
            putStrLn "------------------------------------------------------------"
            -- putStrLn "\ESC[2J"
            putStrLn $ "==> " ++ textbox (State.message state) ++ "\n"
            putStr "> "; hFlush stdout
        input state = do
            str <- getLine
            loop (update state $ string_to_command str)

textbox_width = 50::Int
textbox_buffer = "    "

textbox :: String -> String
textbox string = helper string 0
    where
        helper :: String -> Int -> String
        helper s i = case s of
            [] -> ""
            (c:cs) -> if i > textbox_width
                then if c == ' '
                    then c : "\n" ++ textbox_buffer ++ helper cs (0)
                    else c : helper cs (i+1)
                else c : helper cs (i+1)