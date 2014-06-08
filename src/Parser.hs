module Parser ( parse, references {- nur parse exportieren -})
    where

import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Debug.Trace as Log
import           IR
import           Scanner


--references = Map.fromList [("bildRef", "exampleee.de"), ("bildRef2", "exEampleee.de")]
references = Map.empty

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen
parse :: [MDToken] -> Maybe AST
-- Die leere Liste ergibt eine leere Sequenz
parse []                       = Just $ Sequence []

-- Escape Fälle: *,+,-,**
parse (T_SLASH: T_Text t: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ast)) $ parse (T_Text t:xs)
parse (T_SLASH: T_SLASH:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ast)) $ parse (T_Text "\\":xs)


parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_SPACE s : T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse ( T_OLI : xs) = let (elem, rest)= textParse [] xs
                          withoutNL = filter isNewLine elem
                      in  maybe Nothing (\ast -> Just $ addOLI  withoutNL ast 0) $ parse rest
parse (T_ULI : T_SPACE i: xs) = let (elem, rest)= textParse [] xs
                                    withoutNL = filter isNewLine elem
                                in  maybe Nothing (\ast -> Just $ addULI  withoutNL ast 0) $ parse rest
parse (T_SPACE a: T_OLI : xs) = let (elem, rest)= textParse [] xs
                                    withoutNL = filter isNewLine elem
                                in  maybe Nothing (\ast -> Just $ addOLI  withoutNL ast a) $ parse rest
parse (T_SPACE a: T_ULI : T_SPACE i: xs) = let (elem, rest)= textParse [] xs
                                               withoutNL = filter isNewLine elem
                                           in  maybe Nothing (\ast -> Just $ addULI  withoutNL ast a) $ parse rest
parse (T_SPACE level: xs) | level >= 1 =let (code,rest) = span isNewL  xs
                                     in maybe Nothing (\(Sequence ast) -> Just $ Sequence ((CODE $ mdToText code) : ast)) $ parse rest
                          |otherwise = maybe Nothing (\(Sequence ast) -> Just $ Sequence (ast)) $ parse (T_Text " ":xs)
parse xs   = maybe Nothing (\ast -> Just $ addP (P $fst(textParse [] xs)) ast) $ parse $snd(textParse [] xs)

-- Hilfsfunktion, für das Parsen von Zeilen. 
textParse text (T_Text s:xs)= textParse (text++[Te s]) xs
textParse text (T_BOLD:T_Text str:T_BOLD:xs)= textParse (text++[FT str]) xs
textParse text (T_ITALIC:T_Text str:T_ITALIC:xs)= textParse (text++[CT str]) xs

-- ## Referenzes and Images
textParse text (T_OpenSqu: T_Text title: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[REF title address]) xs -- Referenz
textParse text (T_Exclam: T_OpenSqu:T_Text alt: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[IMG alt address]) xs -- Image
textParse text (T_OpenArrow: T_Text address: T_CloseArrow: xs)= textParse (text++[REF address address]) xs -- Image

textParse text (T_OpenSqu: T_Text title: T_CloseSqu: T_OpenSqu: T_Text reference: T_CloseSqu: xs) = do
    let addressMaybe = Map.lookup reference references
    let list = show(Map.size references)
    if Maybe.isJust addressMaybe == True
        then ([REF title (Maybe.fromJust addressMaybe)], xs) -- Referenz bereits in der Map verfügbar.
        else (text++[Te ("DEBUG:"++list)], xs) -- TODO: neuen Parser extra für Refs durchlaufen lassen. und dann auffüllen


textParse text (T_OpenSqu: T_Text title: T_CloseSqu: T_DoublePoint: T_Text address: xs) = do
    let referencesTemp = Map.insert title address references -- Zur Map hinzufügen
    let references = referencesTemp
    let count = Map.size references
    (text++[Te ("DEBUG:"++show(Map.toList references))], xs) -- leer zurück


textParse text l@(T_Newline:T_Newline:xs)= (text,l)
textParse text (T_Newline:xs)= (text++[NL],xs)
textParse text (T_SPACE a:xs)= (text++[Te " "],xs)

-- ##### Escaping
textParse text (T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("["++t++"]")], xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text (T_Exclam:T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("!["++t++"]")], xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text (T_DoublePoint: xs)= ((text++[Te (":")]), xs) -- Falls nur ein Dopppelpunkt auftaucht 
textParse text (T_SLASH: T_ITALIC: xs) = textParse (text++[Te "*"]) xs
textParse text (T_SLASH: T_SLASH: xs) = textParse (text++[Te "\\"]) xs
textParse text (T_SLASH: T_Text t: xs) = textParse (text++[Te ("\\"++t)]) xs
textParse text (T_SLASH: T_BOLD : xs)= textParse (text++[Te "*"]) (T_ITALIC:xs)
textParse text (T_BackQuote: T_BackQuote: xs) = let (code,rest) = span (\x -> not ( isBlackQuote x) )xs
                                                    quoteCount = length(code)+1
                                                    (code2,rest2)= codeParse [] rest quoteCount quoteCount
                                                 in textParse (text++[CODE $ mdToText code2]) ( rest2)

textParse text (T_BackQuote:xs)= let (code,rest) = span isBlackQuote  xs
                                 in textParse (text++[CODE $ mdToText code]) (tail $rest)


textParse text [] = (text,[])



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

--Hilfsfunktion die Inline Code parsed. Wird aufgerufen wenn InlineCode von 2 oder mehr Backquotes umgeben ist.

codeParse :: [MDToken] -> [MDToken]->Int->Int->([MDToken],[MDToken])
codeParse code (T_BackQuote:xs) 0 origcount = (code,xs)
codeParse code (T_BackQuote:xs) count origcount = codeParse code xs (count -1) origcount
codeParse code (s : xs) count origcount =   let diff = origcount-count
                                                codeQuotes = concat(take diff(repeat "`"))
                                            in   codeParse (code++[T_Text codeQuotes ]++[s]) xs origcount origcount

--Hilfsfunktion die Md-Token in Text umwandelt

mdToText (T_H i :xs) = concat (take i (repeat "#"))++ mdToText xs
mdToText (T_SPACE i : xs)= concat(take i(repeat "    ")) ++ mdToText xs
mdToText (T_Text str  : xs)= str ++ mdToText xs
mdToText (T_ITALIC   : xs)= "*" ++ mdToText xs
mdToText (T_BOLD   : xs)= "**" ++ mdToText xs
mdToText (T_OpenSqu   : xs)= "[" ++ mdToText xs
mdToText (T_CloseSqu   : xs)= "]" ++ mdToText xs
mdToText (T_Exclam   : xs)= "!" ++ mdToText xs
mdToText (T_OpenBracket   : xs)= "(" ++ mdToText xs
mdToText (T_CloseBracket   : xs)= ")" ++ mdToText xs
mdToText (T_OpenArrow   : xs)= "<" ++ mdToText xs
mdToText (T_CloseArrow   : xs)= ">" ++ mdToText xs
mdToText (T_BackQuote   : xs)= "'" ++ mdToText xs
mdToText _ = ""

--Hilfsfunktionen die Auf NewLine, Backquote prüfen

isNewL T_Newline = False
isNewL _ = True

isNewLine NL = False
isNewLine _ = True

isBlackQuote T_BackQuote = False
isBlackQuote _ = True



-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen
addP (P seq1) (Sequence (P seq2 : ast)) = Sequence (P (seq1++seq2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)
