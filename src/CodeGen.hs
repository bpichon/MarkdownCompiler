-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: AST -> String
-- eine Sequenz
generateHTML' (Sequence (a:as)) = generateHTML' a ++ "\n" ++ generateHTML' (Sequence as)
-- eine Überschrift
generateHTML' (H i str) = "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">\n"
-- eine ungeordnete Liste
generateHTML' (UL level lis) = "<ul>\n" ++ concat ( map generateHTML' lis )++ "</ul>\n"
-- eine geordnete Liste
generateHTML' (OL level lis) = "<ol>\n" ++ concat (map generateHTML' lis)++ "</ol>\n"

generateHTML' (LI elem) = "<li>" ++concat (map generateHTML' elem) ++"</li>\n"
-- Listenelemente
-- ein Absatz
generateHTML' (P str)  = "<p>" ++ concat (map generateHTML' str)  ++ "</p>\n"
-- alles andere (?) wird für den Moment ignoriert
generateHTML' (Te str) = str

generateHTML' (NL ) = "<br>"
generateHTML' (FT str) = "<strong>"++str++"</strong>"
generateHTML' (CT str) = "<em>"++str++"</em>"
generateHTML' (REF title url) = "<a href=\""++url++"\">"++title++"</a>"
generateHTML' (IMG alt url) = "<img src=\""++url++"\" alt="++alt++" />"
generateHTML' (CODE code) = "<code>"++ code ++"</code>"
generateHTML' _ = ""


