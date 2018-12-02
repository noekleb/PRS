/* Programnavn: htmlwrapperdef.i
   Made by    : Bent Olsby, 17:21 12.05.99
   Funksjoner : for htmlwrapper.p



	Last change:  BO    3 Jun 99   11:28 am
   Endringer  :
     BO, 03.06.99 Ny input parameter it Html;Col - OptFunk
     BO, 01.06.99 Ny funksjon lagt til - Input
     BO, 26.05.99 Ny funksjon lagt til - Trim'

*/

DEF New Global shared VAR h-HtmlWrapper AS HANDLE NO-UNDO.

{1}

IF NOT VALID-HANDLE(h-HtmlWrapper) THEN
  RUN htmlwrapper.p Persistent SET h-HtmlWrapper.


Function Html;Start returns Char
 (INPUT wSeparator   as CHAR,
  INPUT wTittel      as CHAR,
  INPUT wOptFunk     as Char)
 IN h-HtmlWrapper.

Function Html;Head1 returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wWidth       as CHAR,
  INPUT wOptFunk     as CHAR,
  INPUT wBorder      as INT,
  INPUT wCellSpace   as INT,
  Input wBrytLn      as INT,
  INPUT wAntHeadCol  as INT)
 IN h-HtmlWrapper.

Function Html;Head2 returns Char
 (INPUT wDataLinje   as CHAR)
 IN h-HtmlWrapper.


Function Html;ColHead returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wFormatLinje as Char)
 IN h-HtmlWrapper.

Function Html;Col returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wOptFunk     as CHAR,
  INPUT wLinjeNr     as INT)
 IN h-HtmlWrapper.

Function Html;ColFooter returns Char
 (INPUT wDataLinje   as CHAR)
 IN h-HtmlWrapper.

Function Html;Footer2 returns Char
 (INPUT wDataLinje   as CHAR)
 IN h-HtmlWrapper.

Function Html;Footer1 RETURNS Char
 (INPUT wDataLinje   as CHAR)
 IN h-HtmlWrapper.

Function Html;Space returns Char
 (INPUT wAntSpace    as Int)
 IN h-HtmlWrapper.

Function Html;Skip returns Char
 (INPUT wHeight      as INT)
 IN h-HtmlWrapper.

Function Html;End returns Char
  ()
 IN h-HtmlWrapper.

 Function Html;Trim returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wSeparator   as Char)
 IN h-HtmlWrapper.

 Function Html;Input returns Char
 (INPUT wType        as CHAR,
  INPUT wName        as CHAR,
  INPUT wOrder       as Int,
  INPUT wOptFunk     as Char)
 IN h-HtmlWrapper.

