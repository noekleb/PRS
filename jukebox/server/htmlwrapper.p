/* Programnavn: htmlvrapper.i
   Made by    : Software Service, Bent Olsby, 18:21 06.05.99
   System     : VB
   Funksjoner : HTML Format funksjoner til bruk i utskrifter

   Interface  : htmlwrapperdef.i

     Last change:  BO    8 Jun 99    3:07 pm
   Endringer  :
     BO 08.06.99  Html;Head1 - Optfunk delt opp med separator
     BO 04.06.99  Html;Skip - Fjernet Colspan
     BO 03.06.99  Ny input parameter it Html;Col - OptFunk
     BO 02.06.99  Rettet loop feil i Footer1, Footer2 og ColFooter
     BO 01.06.99  Ny funksjon HTML;Input
     BO 26.05.99  Ny funksjon HTML;Trim(). Fjernet fast høyde på Footer1
     BO 20.05.99  Ny funksjon HTML;Space(),
     RKL, Nov-99  Nye funksjoner: Html;Table Legger CAPTION over tabelllabel
                                             og nullstiller antall colonner
                                  Html;RowAlign Setter Alignment etter at 
                                                tabellen er definert
                                  Html;Row   Brukes til † skrive ut rader p†
                                             formen Label Value Label Value ...
                                             med BOLD p† annethvert felt.
                                  HTML;CVInit Gir copyright p† egen linje f›r
                                              rapportheader med innstallasjon.

*/


&Scoped Version HtmlWrapper V. 1.05

&Scoped UtStream Stream Ut

&Scoped PageBgColor    #BFBFBF
/*&Scoped PageBgColor    #A4DBAD  ---- previous */
/*&Scoped PageBgColor    #b3d0d0  ---- original */

&Scoped TableBgColor   #ffffff

&Scoped Head1BgColor   #E5E5E5
&Scoped Head2BgColor   #E5E5E5 
/*&Scoped Head1BgColor   #317D3F  ---- previous
&Scoped Head2BgColor   #368B45 */
/*&Scoped Head1BgColor   #008080  ---- original
&Scoped Head2BgColor   #009090 */

&Scoped Foot2BgColor   #00a0a0
&Scoped ColHeadBgColor #b0b0b0
&Scoped Data2BgColor   #e0e0e0

&Scoped Font1Color     #005B66
/* &Scoped Font1Color     #ffffff  ---- original */

&Scoped Font2Color     #005B66
/* &Scoped Font2Color     #ffffff  ---- original */

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


RETURN ("{&Version}":U).

/* Internal Functionns */

FUNCTION Html;Format returns CHAR
 (Input wDataLinje as Char):
DEF VAR wSpace as CHAR NO-UNDO.

  Assign
    wDataLinje = REPLACE(wDataLinje,"<HTML:BreakLn>":U  ,"<BR>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:Space>":U    ,"&nbsp;":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:CenterOn>":U ,"<Center>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:CenterOff>":U,"</Center>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:BoldOn>":U   ,"<Strong>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:BoldOff>":U  ,"</Strong>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:BigOn>":U    ,"<Big>":U)
    wDataLinje = REPLACE(wDataLinje,"<HTML:BigOff>":U   ,"</Big>":U).
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
      '<html>':U + NL +
      '<head>':U + NL +
      '  <title>':U + wDataLinje + '</title>':U + NL +
      '</head>':U + NL + NL +
      '<body BGCOLOR="{&PageBgColor}">':U + NL +
      '<font size="2" FACE="Times">':U + NL + wOptFunk + NL
/*       '<font FACE="Arial">':U + NL + wOptFunk + NL */
      .

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
    wWidth       = if wWidth <> "" THEN ' width="':U + wWidth + '"':U else ""
    wColspan     = if wAntHeadCol > 0 THEN ' Colspan="':U + STRING(wAntHeadCol) + '"':U else "".
    
    HtmlUtskrift =
      '<form':U + (if ENTRY(1,wOptFunk,wSep1) = "" then "" else ' ':U + ENTRY(1,wOptFunk,wSep1)) + '>':U + NL +
      '<p><table border="':U + STRING(wBorder) + '" cellspacing="':U + STRING(wCellSpace) + '" align="center" bgColor="{&TableBgColor}"':U + wWidth +
      (if NUM-ENTRIES(wOptFunk,wSep1) > 1 and ENTRY(2,wOptFunk,wSep1) <> "" then ' ':U + ENTRY(2,wOptFunk,wSep1) else "") + '>':U + NL.

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData    = ENTRY(i,wDataLinje,wSep1)
      wDispData    = if wDispdata = "" then "{&NoData}":U else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>':U + NL +
        '  <td ':U + wColspan + ' bgColor="{&Head1BgColor}" height="60" vAlign="Center"><font color="{&Font1Color}"><Center><Big><Strong>':U + wDispData + '</Strong></Big></center></td>':U + NL +
        '</tr>':U + NL.
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
      wDispData = if wDispdata = "" then "{&NoData}":U else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>':U + NL +
        '  <td':U + wColspan + ' bgColor="{&Head2bgColor}"><font color="{&Font2Color}">':U + wDispData + '</td>':U + NL +
        '</tr>':U + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Heading2 */

Function Html;Head3 returns Char
 (INPUT wDataLinje as CHAR):

  ASSIGN
    wAntRow      = NUM-ENTRIES(wDatalinje,wSep1)
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift = '<tr align="Right"':U.

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData = ENTRY(i,wDataLinje,wSep1)
      wDispData = if wDispdata = "" then "{&NoData}":U else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>':U + NL +
        '  <td':U + wColspan + ' bgColor="{&Head2bgColor}"><font color="{&Font2Color}">':U + wDispData + '</td>':U + NL +
        '</tr>':U + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Heading3 */

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
      '<tr bgColor="{&ColHeadBgColor}" align="Right">':U + NL.

  Do i = 1 TO wAntCol:
    Assign
      wDispData  = if i <= wAntCol then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}":U else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + ",":U ELSE ",":U
      wWidth     = if ENTRY(2,wColFormat) <> "" THEN ' width="':U + ENTRY(2,wColFormat) + '"':U else ""
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R":U)
      wAlign     = if wEntry > 0 then ' align="':U + ENTRY(wEntry,"Left,Center,Right":U) + '"':U else ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td height="30"':U + wWidth + wAlign + '><strong>':U + wDispData + '</strong></td>':U + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>':U + NL.
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
      '<tr align="Right"':U + (if wBrytLinje > 0 AND wLinjeNr Mod wBrytLinje = 0 then ' bgColor="{&Data2BgColor}"':U else '') + '>':U + NL.

  Do i = 1 TO MAX(wAntCol,NUM-ENTRIES(wDataLinje,wSep1)):
    Assign
      wDispData  = if i <= Num-Entries(wDataLinje,wSep1) then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}":U else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + ",":U ELSE ",":U
      wCellFunk  = if NUM-ENTRIES(wOptFunk,wSep1) >= i THEN ENTRY(i,wOptFunk,wSep1) ELSE ""
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R":U)
      wAlign     = if wEntry > 0 then ' align="':U + ENTRY(wEntry,"Left,Center,Right":U) + '"':U else ""
      wStrong    = ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td ':U + wCellFunk + wAlign + '>':U + wStrong + wDispData + '</td>':U + NL.
/*        '  <td':U + wAlign + '>':U + wStrong + wDispData + '</td>':U + NL.*/
  END.

  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>':U + NL.
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
    '<tr bgColor="{&ColHeadBgColor}" align="right">':U + NL.

  Do i = 1 TO MAX(wAntCol,NUM-ENTRIES(wDataLinje,wSep1)):
    Assign
      wDispData  = if i <= NUM-ENTRIES(wDataLinje,wSep1) then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}":U else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + ",":U ELSE ",":U
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R":U)
      wAlign     = if wEntry > 0 then ' align="':U + ENTRY(wEntry,"Left,Center,Right":U) + '"':U else ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td height="25"':U + wAlign + '><strong>':U + wDispData + '</strong></td>':U + NL.
  END.

  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>':U + NL + NL.
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
      wDispData = if ENTRY(i,wDataLinje,wSep1) <> "" THEN ENTRY(i,wDataLinje,wSep1) ELSE "{&NoData}":U

      HtmlUtskrift = HtmlUtskrift +
      '<tr>':U + NL +
      '  <td':U + wColspan + ' bgColor="{&Foot2bgColor}"><font color="{&Font2Color}">':U + wDispData + '</td>':U + NL +
      '</tr>':U + NL.
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
      wDispData = if ENTRY(i,wDataLinje,wSep1) <> "" THEN ENTRY(i,wDataLinje,wSep1) ELSE "{&NoData}":U

      HtmlUtskrift = HtmlUtskrift +
      '<tr>':U + NL +
      '  <td':U + wColspan + ' bgColor="{&Head2BgColor}"><font color="{&Font1Color}">':U + wDispData + '</td>':U + NL +
      '</tr>':U + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</table>':U + NL +
    '</form>':U + NL + NL.
  RETURN (HtmlUtskrift).
END. /* Footer1 */


Function Html;Space returns Char
 (INPUT wAntSpace as Int):

  ASSIGN HtmlUtskrift = "".
  Do i = 1 TO wAntSpace:
    ASSIGN HtmlUtskrift = HtmlUtskrift + '&nbsp;':U.
  END.
  RETURN (HtmlUtskrift).
END. /* Space mellom utskrifter */


Function Html;Skip returns Char
 (INPUT wHeight as Int):

  ASSIGN HtmlUtskrift =
    '<table border="0" cellspacing="0" align="center">':U + NL +
    '<tr>':U + NL +
    '  <td height="':U + STRING(wHeight) + '"':U + '>&nbsp;</td>':U + NL +
    '</tr>':U + NL +
    '</table>':U + NL + NL.
  RETURN (HtmlUtskrift).
END. /* Skip mellom utskrifter */


Function Html;End returns Char ():
  ASSIGN HtmlUtskrift =
    '</font>':U + NL +
    '</body>':U + NL +
    '</html>':U + NL + NL + NL + NL.
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
    HtmlUtskrift = '<input type="':U + wType + '" name="':U + wName +
      '" tabindex="':U + STRING(wOrder) + '" ':U + (if wOptFunk = "" then "" else ' ':U + wOptFunk) + '>':U.
  Return (HtmlUtskrift).
End. /* Html;Input */


Function Html;Table returns Char
 (INPUT wCaption      as CHAR,
  INPUT wDataLinje    as CHAR,       /* Tittel */
  INPUT wWidth        as CHAR,
  INPUT wOptFunk      as CHAR,
  INPUT wBorder       as INT,
  INPUT wCellSpace    as INT,
  Input wBrytLn       as INT,
  INPUT wAntHeadCol   as INT):

  ASSIGN
    wAntCol      = 0
    wAntRow      = NUM-ENTRIES(wDataLinje,wSep1)
    wBrytLinje   = wBrytLn
    wDatalinje   = HTML;Format(wDataLinje)
    wWidth       = if wWidth <> "" THEN ' width="':U + wWidth + '"':U else ""
    wColspan     = if wAntHeadCol > 0 THEN ' Colspan="':U + STRING(wAntHeadCol) + '"':U else "".
    
    HtmlUtskrift = 
      '<form':U + (if ENTRY(1,wOptFunk,wSep1) = "" then "" else ' ':U + ENTRY(1,wOptFunk,wSep1)) + '>':U + NL +
      '<p><table border="':U + STRING(wBorder) + '" cellspacing="':U + STRING(wCellSpace) + '" align="center" bgColor="{&TableBgColor}"':U + wWidth +
      (if NUM-ENTRIES(wOptFunk,wSep1) > 1 and ENTRY(2,wOptFunk,wSep1) <> "" then ' ':U + ENTRY(2,wOptFunk,wSep1) else "") + '>':U + 
      '<CAPTION>':U + wCaption + '</CAPTION>':U +  NL.

  Do i = 1 TO wAntRow:
    ASSIGN
      wDispData    = ENTRY(i,wDataLinje,wSep1)
      wDispData    = if wDispdata = "" then "{&NoData}":U else wDispData

      HtmlUtskrift = HtmlUtskrift +
        '<tr>':U + NL +
        '  <td ':U + wColspan + ' bgColor="{&Head1BgColor}" height="60" vAlign="Center"><font color="{&Font1Color}"><Center><Big><Strong>':U + wDispData + '</Strong></Big></center></td>':U + NL +
        '</tr>':U + NL.
  END.
  ASSIGN HtmlUtskrift = HtmlUtskrift + NL.
  RETURN (HtmlUtskrift).
END. /* Heading1 */

Function Html;RowAlign returns Char
 (INPUT wFormatLinje as Char):

  ASSIGN
    wFormat = wFormatLinje.

  RETURN ("").
END. /* HtmlRowAlign */

Function Html;Row returns Char
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
      '<tr align="Right"':U + (if wBrytLinje > 0 AND wLinjeNr Mod wBrytLinje = 0 then ' bgColor="{&Data2BgColor}"':U else '') + '>':U + NL.

  Do i = 1 TO MAX(wAntCol,NUM-ENTRIES(wDataLinje,wSep1)):
    Assign
      wDispData  = if i <= Num-Entries(wDataLinje,wSep1) then ENTRY(i,wDataLinje,wSep1) ELSE ""
      wDispData  = if wDispData = "" THEN "{&NoData}":U else wDispData
      wColFormat = if NUM-ENTRIES(wFormat,wSep1) >= i THEN ENTRY(i,wFormat,wSep1) + ",":U ELSE ",":U
      wCellFunk  = if NUM-ENTRIES(wOptFunk,wSep1) >= i THEN ENTRY(i,wOptFunk,wSep1) ELSE ""
      wEntry     = LOOKUP(ENTRY(1,wColFormat),"L,C,R":U)
      wAlign     = if wEntry > 0 then ' align="':U + ENTRY(wEntry,"Left,Center,Right":U) + '"':U else ""
      wStrong    = ""
      HtmlUtskrift = HtmlUtskrift +
        '  <td bgColor=':U + (IF i MOD 2 = 1 THEN "{&ColHeadBgColor}":U ELSE "{&Data2BgColor}":U) +
           wCellFunk + wAlign + '>':U + wStrong + wDispData + '</td>':U + NL.
/*        '  <td':U + wAlign + '>':U + wStrong + wDispData + '</td>':U + NL.*/
  END.

  ASSIGN HtmlUtskrift = HtmlUtskrift +
    '</tr>':U + NL.
  RETURN (HtmlUtskrift).
END. /* DataLinje */

Function Html;CVInit returns Char
 (Input wSeparator as CHAR,
  INPUT wDataLinje as CHAR,
  INPUT wOptFunk   as Char):

  ASSIGN
    wSep1        = wSeparator
    wDatalinje   = HTML;Format(wDataLinje)
    HtmlUtskrift =
      '<html>':U + NL +
      '<head>':U + NL +
      '  <title>':U + wDataLinje + '</title>':U + NL +
      '</head>':U + NL + NL +
      '<body BGCOLOR="{&PageBgColor}">':U + NL +
      '<font FACE="Arial">':U + NL + wOptFunk + 
      'CreditView ©':U + NL +
      '<CENTER><H1>':U + wDataLinje + '</H1></CENTER>':U.

  RETURN (HtmlUtskrift).
END. /* HtmlStart*/
