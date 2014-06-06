module Parser ( parse {- nur parse exportieren -} )
    where

import           IR
import           Scanner

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen
parse :: [MDToken] -> Maybe AST
-- Die leere Liste ergibt eine leere Sequenz
parse []                       = Just $ Sequence []

-- Escape Fälle: *,+,-,**
parse (T_SLASH: T_Text t: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ast)) $ parse (T_Text t:xs)
parse (T_SLASH: T_SLASH:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ast)) $ parse (T_Text "\\":xs)
-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz eingeführt wird (wirklich immer?)
parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs
--depraciated
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_SPACE s : T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_SPACE a: T_OLI : xs) = let (elem, rest)= textParse [] xs
                                    withoutNL = filter isNewLine elem
                                in  maybe Nothing (\ast -> Just $ addOLI  withoutNL ast a) $ parse rest
parse (T_SPACE a: T_ULI : T_SPACE i: xs) = let (elem, rest)= textParse [] xs   
                                               withoutNL = filter isNewLine elem
                                              
                                           in  maybe Nothing (\ast -> Just $ addULI  withoutNL ast a) $ parse rest
                            
parse xs   = maybe Nothing (\ast -> Just $ addP (P $fst(textParse [] xs)) ast) $ parse $snd(textParse [] xs)
--parse _ = Just $ Sequence []


isNewLine NL = False
isNewLine _ = True

textParse text (T_Text s:xs)= textParse (text++[Te s]) xs
textParse text (T_BOLD:T_Text str:T_BOLD:xs)= textParse (text++[FT str]) xs
textParse text (T_ITALIC:T_Text str:T_ITALIC:xs)= textParse (text++[CT str]) xs
-- ## Referenzes and Images 
textParse text (T_OpenSqu: T_Text title: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[REF title address]) xs -- Referenz
textParse text (T_Exclam: T_OpenSqu:T_Text alt: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[IMG alt address]) xs -- Image
textParse text (T_OpenArrow: T_Text address: T_CloseArrow: xs)= textParse (text++[REF address address]) xs -- Image


textParse text l@(T_Newline:T_Newline:xs)= (text,l)
textParse text (T_Newline:xs)= (text++[NL],xs)
textParse text (T_SPACE a:xs)= (text++[Te " "],xs)

-- ##### Escaping
textParse text (T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("["++t++"]")], xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text (T_Exclam:T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("!["++t++"]")], xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text (T_SLASH: T_ITALIC: xs) = (text++[Te "*"], xs)
textParse text (T_SLASH: T_SLASH: xs) = (text++[Te "\\"], xs)
textParse text (T_SLASH: T_Text t: xs) = (text++[Te ("\\"++t)], xs)
textParse text (T_SLASH: T_BOLD : xs)= (text++[Te "**"], xs)



textParse text [] = (text,[])



-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!




-- Hilfsfunktionen für den Parser

-- Einfügen eines Listenelements in eine ungeordnete Liste
addULI :: [AST] -> AST->Int -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addULI content (Sequence (ul@(UL listLevel lis@(levl)) : ast)) itemLevel
    | (itemLevel == listLevel)  = Sequence (UL listLevel ((LI content):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence ((UL itemLevel ((LI content):[ul] )):ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (UL listLevel ((UL itemLevel [(LI content)]):lis):ast)  -- neue Liste (mit neuem Item) in die Liste

addULI content (Sequence (ul@(OL listLevel lis@(levl)) : ast)) itemLevel
    |(itemLevel == listLevel)  = Sequence (UL listLevel ((LI content):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence ((UL itemLevel ((LI content):[ul] )):ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (UL listLevel ((UL itemLevel [(LI content)]):lis):ast)  -- neue Liste (mit neuem Item) in die Liste
addULI content (Sequence ast)  itemLevel=  Sequence ((UL itemLevel [(LI content)]):ast)

addOLI :: [AST] -> AST->Int -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addOLI content (Sequence (ul@(OL listLevel lis@(levl)) : ast)) itemLevel
    | (itemLevel == listLevel)  = Sequence (OL listLevel ((LI content):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence ((OL itemLevel ((LI content):[ul] )):ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (OL listLevel ((OL itemLevel [(LI content)]):lis):ast)  -- neue Liste (mit neuem Item) in die Liste

addOLI content (Sequence (ul@(UL listLevel lis@(levl)) : ast)) itemLevel
    |(itemLevel == listLevel)  = Sequence (OL listLevel ((LI content):lis) : ast) -- in diese Liste als letztes Element
    | (itemLevel < listLevel)   = Sequence ((OL itemLevel ((LI content):[ul] )):ast) -- eine ebene raus
    | itemLevel > listLevel    = Sequence (OL listLevel ((OL itemLevel [(LI content)]):lis):ast)  -- neue Liste (mit neuem Item) in die Liste
addOLI content (Sequence ast)  itemLevel=  Sequence ((OL itemLevel [(LI content)]):ast)







-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P seq1) (Sequence (P seq2 : ast)) = Sequence (P (seq1++seq2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)
