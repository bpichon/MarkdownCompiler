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
--depraciated
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_SPACE s : T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_SPACE a: T_OLI : T_Text str: xs) = maybe Nothing (\ast -> Just $ addOLI (LI a str ) ast) $ parse xs
parse (T_SPACE a: T_ULI : T_SPACE i: T_Text str: xs) = maybe Nothing (\ast -> Just $ addULI (LI a str ) ast) $ parse xs
--parse (T_BOLD:T_Text str:T_BOLD:xs)= maybe Nothing (\ast -> Just $ addP (P (FT str:[])) ast) $ parse xs
parse xs   = maybe Nothing (\ast -> Just $ addP (P $fst(textParse [] xs)) ast) $ parse $snd(textParse [] xs)
--parse _ = Just $ Sequence []


textParse text (T_Text s:xs)= textParse (text++[Te s]) xs
textParse text (T_BOLD:T_Text str:T_BOLD:xs)= textParse (text++[FT str]) xs
textParse text l@(T_Newline:T_Newline:xs)= (text,l)
textParse text (T_Newline:xs)= (text++[NL],xs)
textParse text (T_SPACE a:xs)= (text++[Te " "],xs)


textParse text [] = (text,[])

textParseErg str rest = (str,rest)

-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!




-- Hilfsfunktionen für den Parser

-- Einfügen eines Listenelements in eine ungeordnete Liste
addULI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addULI li@(LI itemLevel str ) (Sequence (ul@(UL listLevel lis@(levl)) : ast))
    | (itemLevel == listLevel)  = Sequence (UL listLevel ((LI itemLevel str ):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence (UL itemLevel [li,ul] : ast) -- eine ebene raus
    {-| (itemLevel < listLevel)   = Sequence (UL listLevel [(LI itemLevel ("iLevel: "++show(itemLevel)++" listLevel: "++show(listLevel)))] : ast) -- eine ebene raus-}
    | itemLevel > listLevel    = Sequence (UL listLevel ((UL itemLevel [(LI itemLevel str )]):lis):ast) -- neue Liste (mit neuem Item) in die Liste
{- = Sequence ((UL itemLevel [(LI itemLevel str)]):ast)-}

addULI li@(LI itemLevel str ) (Sequence (ul@(OL listLevel lis@(levl)) : ast))
    | (itemLevel == listLevel)  = Sequence (UL listLevel ((LI itemLevel str ):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence (UL itemLevel [li,ul] : ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (UL listLevel ((UL itemLevel [(LI itemLevel str )]):lis):ast) -- neue Liste (mit neuem Item) in die Liste

addULI (LI itemLevel str ) (Sequence ast) = 
    Sequence ((UL itemLevel [(LI itemLevel str )]):ast)


addOLI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addOLI li@(LI itemLevel str ) (Sequence (ul@(OL listLevel lis) : ast))
    | (itemLevel == listLevel)  = Sequence (OL listLevel ((LI itemLevel str ):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence (OL itemLevel [li,ul] : ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (OL listLevel ((OL itemLevel [(LI itemLevel str )]):lis):ast) -- neue Liste (mit neuem Item) in die Liste
addOLI li@(LI itemLevel str ) (Sequence (ul@(UL listLevel lis) : ast))
    | (itemLevel == listLevel)  = Sequence (OL listLevel ((LI itemLevel str ):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence (OL itemLevel [li,ul] : ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (OL listLevel ((OL itemLevel [(LI itemLevel str )]):lis):ast) -- neue Liste (mit neuem Item) in die Liste


addOLI (LI itemLevel str ) (Sequence ast) = 
    Sequence ((OL itemLevel [(LI itemLevel str )]):ast)

    


    
-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P seq1) (Sequence (P seq2 : ast)) = Sequence (P (seq1++seq2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)
