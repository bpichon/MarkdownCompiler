-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR
import Parser
import qualified Data.Map    as Map

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: (AST, References) -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: (AST, References) -> String
-- eine Sequenz
generateHTML' ((Sequence (a:as)),r) = generateHTML' (a,r) ++ "\n" ++ generateHTML' (Sequence as, r)
-- eine Überschrift
generateHTML' ((H i str),_) = "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">\n"
-- eine ungeordnete Liste
generateHTML' ((UL level lis),r) = "<ul>\n" ++ (listGen lis r)++ "</ul>\n"
-- eine geordnete Liste
generateHTML' ((OL level lis),r) = "<ol>\n" ++ listGen lis r++ "</ol>\n"

generateHTML' ((LI elem),r) = "<li>" ++listGen  elem r++"</li>\n"
-- Listenelemente
-- ein Absatz
generateHTML' ((P str),r)  = "<p>" ++ listGen str r  ++ "</p>\n"
-- alles andere (?) wird für den Moment ignoriert
generateHTML' ((Te str),_) = str

generateHTML' ((NL ),_) = "<br>"
generateHTML' ((FT str),_) = "<strong>"++str++"</strong>"
generateHTML' ((CT str),_) = "<em>"++str++"</em>"
generateHTML' ((REF title url),_) =  "<a href=\""++url++"\">"++title++"</a>"                                           
generateHTML' ((REF2 title url),r) =  let mx = Map.lookup url r                                         
                                     in  case mx of
                                     Just a -> "<a href=\""++a++"\">"++title++"</a>"      
                                     Nothing -> "<a href=\""++show r++"\">"++title++"</a>"
generateHTML' ((IMG2 alt url),r) =  let mx = Map.lookup url r                                         
                                     in  case mx of
                                     Just a -> "<img src=\""++a++"\" alt="++alt++" />"
                                     Nothing -> "<img src=\""++url++"\" alt="++alt++" />"
-- <a href="http://search.yahoo.com/" title="Yahoo Search">Yahoo</a>

generateHTML' ((IMG alt url),_) = "<img src=\""++url++"\" alt="++alt++" />"
generateHTML' ((CODE code),_) = "<code>"++ code ++"</code>"
generateHTML' (_,_) = ""

listGen [] rs = ""
listGen (elem:xs) rs= generateHTML' (elem,rs)++ listGen xs rs
