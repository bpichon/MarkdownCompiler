module IR where

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST] -- eine Sequenz von HTML-Elementen
         | H Int [Char]   -- eine Überschrift, de Int ist das Level (6 für H6) und der String der Text
         | UL Int [AST]       -- eine ungeordnete Liste, in der Liste müssen dann die Listenelemente stehen
         | OL Int [AST] -- eine geordnete Liste
         | LI [AST] -- ein Listenelement mit dem Inhalt 
         | P [AST]       -- ein Absatz mit dem Inhalt
         | EmptyLine      -- eine leere Zeile
         | Te [Char]      --normaler Text
         | FT [Char]    -- Fetter Text
         | CT [Char]    -- Kursiver Text
         | NL           --Neue Zeile
         | REF [Char] [Char] -- Referenz auf eine andere Seite.
         | REF2 [Char] [Char] -- Referenz auf eine andere Seite.
         | IMG [Char] [Char] -- Bild mit Alt-Text und Bildadresse
         | IMG2 [Char] [Char] -- Bild mit Alt-Text und Bildadresse
         | CODE [Char] -- Code enthält kein Markdown
    deriving (Show)
