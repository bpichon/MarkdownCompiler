module Main where

import           CodeGen
import           IR
import           Parser
import           Scanner
import qualified Data.Maybe  as Maybe

main :: IO ()
main = do
    -- lese den Inhalt der Datei "test.md" als einen kompletten String ein
    input <- readFile "test.md"
    -- versuche den String zu scannen
    let maybeTokens = scan input
    putStrLn "Scanner output\n=============="
    print maybeTokens
    -- the parse
    case maybeTokens of
        Nothing -> putStrLn "scanner failed"
        Just tokens -> do -- der Scanner war erfolgreich
            -- versuche die Tokens zu parsen
            let maybeAst = parse tokens
                (ast, refMap) = Maybe.fromJust(maybeAst)
            putStrLn "\nParser output\n============="
            print maybeAst--refMap
            case maybeAst of
                Nothing -> putStrLn "parser failed"
                Just ast -> do -- der Parser war erfolgreich
                    putStrLn "\nGenerated HTML\n=============="
                    -- generiere HTML und gebe es aus
                    putStrLn $ generateHTML ast 
