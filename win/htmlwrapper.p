/* Programnavn: htmlvrapper.i
   Made by    : Software Service, Bent Olsby, 18:21 06.05.99
   Funksjoner : HTML Format funksjoner til bruk i utskrifter

   Interface  : htmlwrapperdef.i

     Last change:  TN   17 Nov 99   11:29 pm
   Endringer  :
     BO 08.06.99  Html;Head1 - Optfunk delt opp med separator
     BO 04.06.99  Html;Skip - Fjernet Colspan
     BO 03.06.99  Ny input parameter it Html;Col - OptFunk
     BO 02.06.99  Rettet loop feil i Footer1, Footer2 og ColFooter
     BO 01.06.99  Ny funksjon HTML;Input
     BO 26.05.99  Ny funksjon HTML;Trim(). Fjernet fast høyde på Footer1
     BO 20.05.99  Ny funksjon HTML;Space(),

*/


&Scoped Version HtmlWrapper V. 1.05

&Scoped UtStream Stream Ut

&Scoped PageBgColor    #b3d0d0
&Scoped TableBgColor   #ffffff
&Scoped Head1BgColor   #008080
&Scoped Head2BgColor   #009090
&Scoped Foot2BgColor   #00a0a0
&Scoped ColHeadBgColor #b0b0b0
&Scoped Data2BgColor   #e0e0e0
&Scoped Font1Color     #ffffff
&Scoped Font2Color     #ffffff
&Scoped NoData         &nbsp;

DEF VAR HtmlUtskrift   as Char NO-UNDO.
DEF VAR wColSpan       as Char NO-UNDO.
DEF VAR wDispData      as Char NO-UNDO.
DEF VAR wFormat        as char NO-UNDO.
DEF VAR wSep1          as char NO-UNDO.
DEF VAR NL             as CHAR NO-UNDO. /* New Line */
DEF VAR wBrytLinje     as INT  NO-UNDO.
DEF VAR wAntRow        as INT  NO-UNDO.
DEF VAR wAntCol        as INT  NO-UNDO.
DEF VAR wEntry         as INT  NO-UNDO.
DEF VAR i              as INT  NO-UNDO.

ASSIGN NL = CHR(10). /* Windows Cr+LF */

RETURN ("{&Version}").

/* Internal Functionns */

FUNCTION Html;Format returns CHAR
 (Input wDataLinje as Char):
DEF VAR wSpace as CHAR NO-UNDO.

  Assign
    wDataLinje = REPLACE(wDataLinje,"<HTML:BreakLn>"  ,"<BR>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:Space>"    ,"&nbsp;")
    wDataLinje = REPLACE(wDataLinje,"<HTML:CenterOn>" ,"<Center>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:CenterOff>","</Center>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:BoldOn>"   ,"<Strong>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:BoldOff>"  ,"</Strong>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:BigOn>"    ,"<Big>")
    wDataLinje = REPLACE(wDataLinje,"<HTML:BigOff>"   ,"</Big>").
  RETURN (wDataLinje).
END.

/* External Functionns */

Function Html;Start returns Char
 (Input wSeparator as CHAR,
  INPUT wDataLinje as CHAR,
  INPUT wOptFunk   as Char):

  ASSIGN
    wSep1        = wSeparator
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift =
      '<html>' + NL +
      '<head>' + NL +
      '  <title>' + wDataLinje + '</title>' + NL +
      '</head>' + NL + NL +
      '<body BGCOLOR="{&PageBgColor}">' + NL +
      '<font FACE="Arial">' + NL + wOptFunk + NL.

  RETURN (HtmlUtskrift).
END. /* HtmlStart*/


Function Html;Head1 returns Char
 (INPUT wDataLinje    as CHAR,       /* Tittel */
  INPUT wWidth        as CHAR,
  INPUT wOptFunk      as CHAR,
  INPUT wBorder       as INT,
  INPUT wCellSpace    as INT,
  Input wBrytLn       as INT,
  INPUT wAntHeadCol   as INT):

  ASSIGN
    wAntRow      = NUM-ENTRIES(wDataLinje,wSep1)
    wBrytLinje   = wBrytLn
    wDatalinje   = HTML;Format(wDataLinje)
    wWidth       = if wWidth <> "" THEN ' width="' + wWidth + '"' else ""
    wColspan     = if wAntHeadCol > 0 THEN ' Colspan="' + STRING(wAntHeadCol) + '"' else "".
    
    HtmlUtskrift =
      '<form' + (if ENTRY(1,wOptFunk,wSep1) = "" then "" else ' ' + ENTRY(1,wOptFunk,wSep1)) + '>' + NL +
      '<p><table border="' + STRING(wBorder) + '" cellspacing="' + STRING(wCellSpace) + '" align="center" bgColor="{&TableBgColor}"' + wWidth +
      (if NUM-ENTRIES(wOptFunk,wSep1) > 1 and ENTRY(2,wOptFunk,wSep1) <> "" then ' ' + ENTRY(2,wOptFunk,wSep1) else "") + '>' + NL.

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData    = ENTRY(i,wDataLinje,wSep1)
      wDispData    = if wDispdata = "" then "{&NoData}" else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>' + NL +
        '  <td ' + wColspan + ' bgColor="{&Head1BgColor}" height="60" vAlign="Center"><font color="{&Font1Color}"><Center><Big><Strong>' + wDispData + '</Strong></Big></center></td>' + NL +
        '</tr>' + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Heading1 */


Function Html;Head2 returns Char
 (INPUT wDataLinje as CHAR):

  ASSIGN
    wAntRow      = NUM-ENTRIES(wDatalinje,wSep1)
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift = "".

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData = ENTRY(i,wDataLinje,wSep1)
      wDispData = if wDispdata = "" then "{&NoData}" else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>' + NL +
        '  <td' + wColspan + ' bgColor="{&Head2bgColor}"><font color="{&Font2Color}">' + wDispData + '</td>' + NL +
        '</tr>' + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Heading2 */

Function Html;ColHead returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wFormatLinje as Char):

  DEF VAR wColFormat as CHAR NO-UNDO.
  DEF VAR wAlign     as CHAR NO-UNDO.
  DEF VAR wWidth     as CHAR NO-UNDO.

  ASSIGN
    wAntCol      = NUM-ENTRIES(wDatalinje,wSep1)
    wDatalinje   = HTML;Format(wDataLinje)
    wFormat      = wFormatLinje

    HtmlUtskrift =
      '<tr bgColor="{&ColHeadBgColor}" align="Right">' + NL.

  Do i = 1 TO wAntCol:
    Assign
      wDispData  = if i <= wAntCol then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}" else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + "," ELSE ","
      wWidth     = if ENTRY(2,wColFormat) <> "" THEN ' width="' + ENTRY(2,wColFormat) + '"' else ""
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R")
      wAlign     = if wEntry > 0 then ' align="' + ENTRY(wEntry,"Left,Center,Right") + '"' else ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td height="30"' + wWidth + wAlign + '><strong>' + wDispData + '</strong></td>' + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>' + NL.
  RETURN (HtmlUtskrift).
END. /* ColHeading */


Function Html;Col returns Char
 (INPUT wDataLinje as CHAR,
  INPUT wOptFunk   as CHAR,
  INPUT wLinjeNr   as INT):

  DEF VAR wColFormat as CHAR NO-UNDO.
  DEF VAR wAlign     as CHAR NO-UNDO.
  DEF VAR wStrong    as CHAR NO-UNDO.
  DEF VAR wCellFunk  as CHAR NO-UNDO.

  Assign
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift =
      '<tr align="Right"' + (if wBrytLinje > 0 AND wLinjeNr Mod wBrytLinje = 0 then ' bgColor="{&Data2BgColor}"' else '') + '>' + NL.

  Do i = 1 TO MAX(wAntCol,NUM-ENTRIES(wDataLinje,wSep1)):
    Assign
      wDispData  = if i <= Num-Entries(wDataLinje,wSep1) then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}" else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + "," ELSE ","
      wCellFunk  = if NUM-ENTRIES(wOptFunk,wSep1) >= i THEN ENTRY(i,wOptFunk,wSep1) ELSE ""
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R")
      wAlign     = if wEntry > 0 then ' align="' + ENTRY(wEntry,"Left,Center,Right") + '"' else ""
      wStrong    = ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td ' + wCellFunk + wAlign + '>' + wStrong + wDispData + '</td>' + NL.
/*        '  <td' + wAlign + '>' + wStrong + wDispData + '</td>' + NL.*/
  END.

  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>' + NL.
  RETURN (HtmlUtskrift).
END. /* DataLinje */


Function Html;ColFooter returns Char
 (INPUT wDataLinje   as CHAR):

  DEF VAR wDispData  as Char NO-UNDO.
  DEF VAR wColFormat as CHAR NO-UNDO.
  DEF VAR wAlign     as CHAR NO-UNDO.

  Assign
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift =
    '<tr bgColor="{&ColHeadBgColor}" align="right">' + NL.

  Do i = 1 TO MAX(wAntCol,NUM-ENTRIES(wDataLinje,wSep1)):
    Assign
      wDispData  = if i <= NUM-ENTRIES(wDataLinje,wSep1) then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}" else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + "," ELSE ","
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R")
      wAlign     = if wEntry > 0 then ' align="' + ENTRY(wEntry,"Left,Center,Right") + '"' else ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td height="25"' + wAlign + '><strong>' + wDispData + '</strong></td>' + NL.
  END.

  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>' + NL + NL.
  RETURN (HtmlUtskrift).
END. /* ColFooter */


Function Html;Footer2 returns Char
 (INPUT wDataLinje   as CHAR):

  ASSIGN
    wAntRow      = NUM-ENTRIES(wDatalinje,wSep1)
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift = "".

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData = if ENTRY(i,wDataLinje,wSep1) <> "" THEN ENTRY(i,wDataLinje,wSep1) ELSE "{&NoData}"

      HtmlUtskrift = HtmlUtskrift +
      '<tr>' + NL +
      '  <td' + wColspan + ' bgColor="{&Foot2bgColor}"><font color="{&Font2Color}">' + wDispData + '</td>' + NL +
      '</tr>' + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Footer2 */


Function Html;Footer1 RETURNS Char
 (INPUT wDataLinje   as CHAR):

  ASSIGN
    wAntRow      = NUM-ENTRIES(wDatalinje,wSep1)
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift = "".

  Do i = 1 TO wAntRow: /* Vanlighvis kun 1 */
    ASSIGN
      wDispData = if ENTRY(i,wDataLinje,wSep1) <> "" THEN ENTRY(i,wDataLinje,wSep1) ELSE "{&NoData}"

      HtmlUtskrift = HtmlUtskrift +
      '<tr>' + NL +
      '  <td' + wColspan + ' bgColor="{&Head2BgColor}"><font color="{&Font1Color}">' + wDispData + '</td>' + NL +
      '</tr>' + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</table>' + NL +
    '</form>' + NL + NL.
  RETURN (HtmlUtskrift).
END. /* Footer1 */


Function Html;Space returns Char
 (INPUT wAntSpace as Int):

  ASSIGN HtmlUtskrift = "".
  Do i = 1 TO wAntSpace:
    ASSIGN HtmlUtskrift = HtmlUtskrift + '&nbsp;'.
  END.
  RETURN (HtmlUtskrift).
END. /* Space mellom utskrifter */


Function Html;Skip returns Char
 (INPUT wHeight as Int):

  ASSIGN HtmlUtskrift =
    '<table border="0" cellspacing="0" align="center">' + NL +
    '<tr>' + NL +
    '  <td height="' + STRING(wHeight) + '"' + '>&nbsp;</td>' + NL +
    '</tr>' + NL +
    '</table>' + NL + NL.
  RETURN (HtmlUtskrift).
END. /* Skip mellom utskrifter */


Function Html;End returns Char ():
  ASSIGN HtmlUtskrift =
    '</font>' + NL +
    '</body>' + NL +
    '</html>' + NL + NL + NL + NL.
  RETURN (HtmlUtskrift).
END. /* HtmlSlutt */



/* Spesialfunksjoner */
Function Html;Trim returns Char
 (INPUT wDataLinje   as CHAR,
  INPUT wSeparator   as Char):

  Do i = 1 to Num-Entries(wDataLinje,wSeparator):
    Assign Entry(i,wDataLinje,wSeparator) = Trim(Entry(i,wDataLinje,wSeparator)).
  End.
  Return (wDataLinje).
End. /* Html;Trim */


Function Html;Input returns Char
 (INPUT wType        as CHAR,
  INPUT wName        as CHAR,
  INPUT wOrder       as Int,
  INPUT wOptFunk     as Char):

  ASSIGN
    HtmlUtskrift = '<input type="' + wType + '" name="' + wName +
      '" tabindex="' + STRING(wOrder) + '" ' + (if wOptFunk = "" then "" else ' ' + wOptFunk) + '>'.
  Return (HtmlUtskrift).
End. /* Html;Input */


