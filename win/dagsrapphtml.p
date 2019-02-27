 /*

	Last change:  BO    3 Jun 99    3:25 pm
*/


&Scoped UtStream Stream Ut
DEF STREAM Ut.

DEF VAR wLinje AS CHAR NO-UNDO.
DEF VAR wLnNr  AS INT  NO-UNDO.
DEF VAR wt     AS CHAR NO-UNDO.
DEF VAR j      AS INT  NO-UNDO.
DEF VAR wNr    AS INT  NO-UNDO.
DEF VAR wHit AS LOGI NO-UNDO.
DEF VAR ii AS INTE NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR wHead2       AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER ch_Grid      AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER wDato        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wRapportdel  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wtmpFileName AS CHAR NO-UNDO.

DEFINE VAR             wSep         AS CHAR INIT "|" NO-UNDO.
DEFINE VAR             wCount       AS INTE NO-UNDO.
DEFINE VAR             wColHeadForm AS CHAR NO-UNDO.

DEFINE VAR wHead1       AS CHAR NO-UNDO.
DEFINE VAR wTitle       AS CHAR NO-UNDO.

{htmlwrapperdef.i }

ASSIGN wHead1 = "Dagsrapport" + " " + wDato.
Output {&UtStream} to VALUE(wtmpFileName).
DO:
    PUT {&UtStream} Unformatted
        HTML;Start (wSep,"wTitle","")
        HTML;Head1 (wHead1,"100%","",1,0,ch_Grid:Rows - 2,8)
        HTML;Head2 (ENTRY(1,wRapportdel,wSep)).
    ASSIGN wLnNr = 0.
    DO wCount = 0 TO ch_Grid:Rows - 3:          
        ASSIGN wLnNr  = wLnNr + 1
               wLinje = IF wCount = 0 THEN 
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            ch_Grid:TextMatrix(wCount,1) + wSep +
                            ch_Grid:TextMatrix(wCount,2) + wSep +
                            ch_Grid:TextMatrix(wCount,3) + wSep +
                            ch_Grid:TextMatrix(wCount,4) + wSep +
                            ch_Grid:TextMatrix(wCount,5) + wSep +
                            ch_Grid:TextMatrix(wCount,6) + wSep +
                            ch_Grid:TextMatrix(wCount,19)
                        ELSE
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,1)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,2)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,3)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,4)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,5)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,6)),"->>>,>>>,>>9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,19)),0),"->>>,>>>,>>9").
        IF wCount = 0 THEN
            PUT {&UtStream} Unformatted HTML;ColHead (wLinje,"R,R,R,R,R,R,R,R"). 
        ELSE
            PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
    END.
    PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (0).

    PUT {&UtStream} Unformatted
        HTML;Head1 ("","100%","",1,0,ch_Grid:Rows - 2,8)
        HTML;Head2 (ENTRY(2,wRapportdel,wSep)).
    ASSIGN wLnNr = 0.
    DO wCount = 0 TO ch_Grid:Rows - 3:          
        ASSIGN wLnNr  = wLnNr + 1
               wLinje = IF wCount = 0 THEN
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            ch_Grid:TextMatrix(wCount,7) + wSep +
                            ch_Grid:TextMatrix(wCount,8) + wSep +
                            ch_Grid:TextMatrix(wCount,9) + wSep +
                            ch_Grid:TextMatrix(wCount,10) + wSep +
                            ch_Grid:TextMatrix(wCount,11) + wSep +
                            ch_Grid:TextMatrix(wCount,12) + wSep +
                            ch_Grid:TextMatrix(wCount,20)
                        ELSE
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,7)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,8)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,9)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,10)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,11)),"->>>,>>>,>>9") + wSep +
                            STRING(INT(ch_Grid:TextMatrix(wCount,12)),"->>>,>>>,>>9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,20)),0),"->>>,>>>,>>9").
        IF wCount = 0 THEN
            PUT {&UtStream} Unformatted HTML;ColHead (wLinje,"R,R,R,R,R,R,R,R"). 
        ELSE
            PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
    END.
    PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (0).

    PUT {&UtStream} Unformatted
        HTML;Head1 ("","100%","",1,0,ch_Grid:Rows - 2,8)
        HTML;Head2 (ENTRY(3,wRapportdel,wSep)).
    ASSIGN wLnNr = 0.
    DO wCount = 0 TO ch_Grid:Rows - 3:          
        ASSIGN wLnNr  = wLnNr + 1
               wLinje = IF wCount = 0 THEN
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            ch_Grid:TextMatrix(wCount,13) + wSep +
                            ch_Grid:TextMatrix(wCount,14) + wSep +
                            ch_Grid:TextMatrix(wCount,15) + wSep +
                            ch_Grid:TextMatrix(wCount,16) + wSep +
                            ch_Grid:TextMatrix(wCount,17) + wSep +
                            ch_Grid:TextMatrix(wCount,18) + wSep +
                            ch_Grid:TextMatrix(wCount,21)
                        ELSE
                            ch_Grid:TextMatrix(wCount,0) + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,13)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,14)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,15)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,16)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,17)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,18)),1),"->,>>9.9") + wSep +
                            STRING(ROUND(DECI(ch_Grid:TextMatrix(wCount,21)),1),"->,>>9.9").
        IF wCount = 0 THEN
            PUT {&UtStream} Unformatted HTML;ColHead (wLinje,"R,R,R,R,R,R,R,R"). 
        ELSE
            PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
    END.
    PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (1).
END.
PUT {&UtStream} Unformatted
    HTML;END     ().
