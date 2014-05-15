module Parser ( parse {- nur parse exportieren -} )
    where

import           IR
import           Scanner

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen 
parse :: [MDToken] -> Maybe AST
-- Die leere Liste ergibt eine leere Sequenz
parse []                       = Just $ Sequence []
-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz eingeführt wird (wirklich immer?)
parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs
-- ein einzelnes Leerzeichen ignorieren wir (für den Moment?)
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_SPACE s : T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_SPACE a: T_ULI: T_SPACE i: T_Text str: xs) = maybe Nothing (\ast -> Just $ addULI (LI a str) ast) $ parse xs



-- ein Text am Anfang gehört in einen Absatz. Damit direkt auf einander folgende Texte in einem gemeinsamen
-- Absatz landen, wird die Hilfsfunktion addP genutzt um den Text einzufügen
parse (T_Text str: xs)         = maybe Nothing (\ast -> Just $ addP (P str) ast) $ parse xs
-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
parse _ = Just $ Sequence []

-- Hilfsfunktionen für den Parser

-- Einfügen eines Listenelements in eine ungeordnete Liste
addULI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addULI (LI itemLevel str) (Sequence (UL listLevel lis : ast))
    | (itemLevel == listLevel)  = Sequence (UL listLevel ((LI itemLevel str):lis) : ast)
    | (itemLevel < listLevel)   = Sequence (UL listLevel [(LI itemLevel str)] : ast)
    | itemLevel > listLevel    = Sequence (UL listLevel ((UL itemLevel [(LI itemLevel str)]):lis):ast)
{- = Sequence ((UL itemLevel [(LI itemLevel str)]):ast)-}

addULI (LI itemLevel str) (Sequence ast) = 
    Sequence ((UL itemLevel [(LI itemLevel str)]):ast)

-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P str1) (Sequence (P str2 : ast)) = Sequence (P (str1 ++ "\n" ++ str2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)
