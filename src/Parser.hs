module Parser ( parse, References {- nur parse exportieren -})
    where

import qualified Data.Map    as Map
import qualified Data.Maybe  as Maybe
import qualified Debug.Trace as Log
import           IR
import           Scanner

type Key         = String
type Address     = String
type References  = Map.Map Key Address

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen
parse :: [MDToken] -> Maybe (AST, References)
-- Die leere Liste ergibt eine leere Sequenz
parse []                       = Just $ (Sequence [], Map.empty)

-- Escape Fälle: *,+,-,**
parse (T_SLASH: T_Text t: xs) = maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence (ast), refs)) $ parse (T_Text t:xs)
parse (T_SLASH: T_SLASH:xs) = maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence (ast), refs)) $ parse (T_Text "\\":xs)


parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence (EmptyLine : ast), refs)) $ parse xs
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_SPACE s : T_Text str: xs) = let (header,rest) = span isNewL  xs
                                                 text  = str ++ ( mdToText header)
                                             in   maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence (H i text:ast), refs)) $ parse rest
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse ( T_OLI : xs) = let (elem, refs, rest)= textParse [] Map.empty  xs
                          withoutNL = filter isNewLine elem
                      in  maybe Nothing (\(ast, refs) -> Just $ (addOLI  withoutNL ast 0, refs)) $ parse rest
parse (T_ULI : T_SPACE i: xs) = let (elem, reference, rest)= textParse [] Map.empty  xs
                                    withoutNL = filter isNewLine elem
                                in  maybe Nothing (\(ast, refs) -> Just $ (addULI  withoutNL ast 0, Map.union reference refs)) $ parse rest
parse (T_SPACE a: T_OLI : xs) = let (elem, reference, rest)= textParse [] Map.empty  xs
                                    withoutNL = filter isNewLine elem
                                in  maybe Nothing (\(ast, refs) -> Just $ ((addOLI  withoutNL ast a), Map.union reference refs)) $ parse rest
parse (T_SPACE a: T_ULI : T_SPACE i: xs) = let (elem, reference, rest)= textParse [] Map.empty  xs
                                               withoutNL = filter isNewLine elem
                                           in  maybe Nothing (\(ast, refs) -> Just $  ((addULI  withoutNL ast a), Map.union reference refs)) $ parse rest
parse (T_SPACE level: xs) | level >= 1 =let (code,rest) = span isNewL  xs
                                     in maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence ((CODE $ mdToText code) : ast),refs)) $ parse rest
                          |otherwise = maybe Nothing (\(Sequence ast, refs) -> Just $ (Sequence (ast), refs)) $ parse (T_Text " ":xs)
parse xs   =   let (tokens,reference, rest) = textParse [] Map.empty xs
               in   maybe Nothing (\(ast, refs) -> Just $ ((addP (P  (tokens )) ast), Map.union reference refs)) $ parse rest
               
textParse :: [AST]->References->[MDToken]->([AST],References,[MDToken])
-- Hilfsfunktion, für das Parsen von Zeilen.
textParse text refs (T_Text s:xs)= textParse (text++[Te s]) refs  xs
textParse text refs (T_BOLD:T_Text str:T_BOLD:xs)= textParse (text++[FT str]) refs  xs
textParse text refs (T_ITALIC:T_Text str:T_ITALIC:xs)= textParse (text++[CT str]) refs  xs

-- ## Referenzes and Images
textParse text refs (T_OpenSqu: T_Text title: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[REF title address]) refs  xs -- Referenz
textParse text refs (T_Exclam: T_OpenSqu:T_Text alt: T_CloseSqu: T_OpenBracket: T_Text address: T_CloseBracket: xs)= textParse (text++[IMG alt address]) refs  xs -- Image
textParse text refs (T_OpenArrow: T_Text address: T_CloseArrow: xs)= textParse (text++[REF address address]) refs  xs -- Image

textParse text refs (T_OpenSqu: T_Text title: T_CloseSqu: T_OpenSqu: T_Text referenceFirst:  T_CloseSqu: xs) = textParse (text++[REF2 title referenceFirst]) refs  xs

textParse text refs (T_OpenSqu: T_Text title: T_CloseSqu: T_DoublePoint: T_Text address: T_DoublePoint:T_Text address2: xs) =
    let references = Map.insert title (address++":"++address2) refs -- Zur Map hinzufügen
    in  textParse text references xs


textParse text refs l@(T_Newline:T_Newline:xs)= (text,refs ,l)


textParse text refs (T_Newline:xs)= (text++[NL],refs ,xs)

textParse text refs (T_SPACE a:xs)= (text++[Te " "],refs,xs)

-- ##### Escaping
textParse text refs (T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("["++t++"]")], refs ,xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text refs (T_Exclam:T_OpenSqu: T_Text t: T_CloseSqu: xs)= (text++[Te ("!["++t++"]")], refs , xs) -- Falls nur eckige Klammern auftauchen ohne adressangabe
textParse text refs (T_DoublePoint: xs)= textParse (text++[Te ":"]) refs xs -- Falls nur ein Dopppelpunkt auftaucht
textParse text refs (T_SLASH: T_ITALIC: xs) = textParse (text++[Te "*"]) refs  xs
textParse text refs (T_SLASH: T_SLASH: xs) = textParse (text++[Te "\\"]) refs  xs
textParse text refs (T_SLASH: T_Text t: xs) = textParse (text++[Te ("\\"++t)]) refs  xs
textParse text refs (T_SLASH: T_BOLD : xs)= textParse (text++[Te "*"]) refs  (T_ITALIC:xs)
textParse text refs (T_BackQuote: T_BackQuote: xs) = let (code,rest) = span (\x -> not ( isBlackQuote x) )xs
                                                         quoteCount = length(code)+1
                                                         (code2,rest2)= codeParse [] rest quoteCount quoteCount
                                                 in textParse (text++[CODE $ mdToText code2]) refs  ( rest2)

textParse text refs (T_BackQuote:xs)= let (code,rest) = span isBlackQuote  xs
                                 in textParse (text++[CODE $ mdToText code]) refs (tail $rest)


textParse text refs [] = (text, refs,[])
-- Alles womit der compiler nicht zurechtkommt wird direkt in text umgewandel
textParse text refs (x:rest)  =textParse (text++[Te (mdToText [x])]) refs rest



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
