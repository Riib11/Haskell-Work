import System.IO
import System.Console.ANSI
import System.Exit

import State

main :: IO ()
main = do
    loop State.state_start
    where
        loop state =
            if running state
                then do
                    prompt state -- display message and prompt
                    input state  -- get user input (parsed to command)
                else do exitSuccess
        
        prompt state = do
            putStrLn ""
            putStrLn "------------------------------------------------------------"
            putStrLn "\ESC[2J"
            putOutputHeader
            putStr "\n"
            putStrLn $ foldl
                (\s x -> s ++ textbox (show x)) "" (notes state) 
            -- putStrLn $ textbox (State.show_notes state) ++ "\n"
            putInputHeader
            hFlush stdout
        
        input state = do
            str <- getLine
            loop (update state $ string_to_command str)

putStrColored clr str = do
    setSGR [SetColor Foreground Vivid clr]
    putStr str
    setSGR [Reset]

putOutputHeader = do
    putStr "\ESC[2J"
    putStrColored Red "◀ "
    putStrColored Magenta " output "
    putStrColored Red "▶  "

putInputHeader = do
    putStrColored Green "◀ "
    putStrColored Cyan " input "
    putStrColored Green "▶  "
