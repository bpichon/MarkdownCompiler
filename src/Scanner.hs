module Scanner where

import Data.Char (isDigit)

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_ULI     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_POINT     -- ein geordnetes Listenelement Marker mit einrückungsebene.
             | T_INT Int     -- Zahl nach token
             | T_SPACE Int
    deriving (Show, Eq)

scan :: String -> Maybe [MDToken]
-- Rekursionsende
scan ""           = Just []
-- eine Überschrift
scan str@('#':xs) =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (=='#') str
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest
-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen
scan ('\n':xs)    = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs

scan str@(' ':xs) =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (==' ') str
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = (length hashes) `div` 4
    in maybe Nothing (\tokens -> Just (T_SPACE level:tokens))      $ scan rest

-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 0
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':xs)     = maybe Nothing (\tokens -> Just (T_ULI:tokens))    $ scan xs

-- punkt für Geordnete Liste
scan ('.':xs)     = maybe Nothing (\tokens -> Just (T_POINT:tokens))    $ scan xs

-- sonst lesen wir einfach den Rest bis zum Zeilenende in ein Text-Token ein

-- Wenn eine Zahl am anfang steht
scan str@(x:xs)
    | isDigit x = let (digits, rest) = span isDigit str
                  in maybe Nothing (\tokens -> Just (T_INT (read digits):tokens))(scan xs)
    | otherwise = let (restOfLine, restOfStr) = span (/='\n') str
          in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr


