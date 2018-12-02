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

DEFINE INPUT PARAMETER wDokTyp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wSep         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead1Set    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHead     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFields      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFormat      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHeadForm AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTabell      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wQY          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTot         AS LOGI NO-UNDO.
DEFINE INPUT PARAMETER hQuery AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER wtmpFileName AS CHAR NO-UNDO.

DEFINE VAR wHead1       AS CHAR NO-UNDO.
DEFINE VAR wTitle       AS CHAR NO-UNDO.

IF VALID-HANDLE(hQuery) THEN DO:
    ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:REPOSITION-TO-ROW(1).
END.
{htmlwrapperdef.i }

ASSIGN wTitle = "Rapport " + wTabell
       wHead1 = wTitle
       wHead2 = STRING(TODAY,"99/99/9999")
       .

Output {&UtStream} to VALUE(wtmpFileName).
IF wDokTyp = "EX" THEN DO:
    PUT {&UtStream} Unformatted IF wSep <> ";" THEN REPLACE(wColHead,wSep,";") ELSE wColHead SKIP.   
END.
ELSE DO:
    PUT {&UtStream} Unformatted
      HTML;Start (wSep,wTitle,"")
      HTML;Head1 (wHead1,ENTRY(1,wHead1Set),ENTRY(2,wHead1Set),INT(ENTRY(3,wHead1Set)),INT(ENTRY(4,wHead1Set)),INT(ENTRY(5,wHead1Set)),INT(ENTRY(6,wHead1Set)))
      HTML;Head2 (wHead2)
      HTML;ColHead (wColHead,"" /* wColHeadForm */). 
END.
ASSIGN wLnNr = 0.
REPEAT:
    IF wLnNr = 0 THEN
        hQuery:GET-FIRST() NO-ERROR.
    ELSE
        hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    Assign wLnNr  = 1
           wLinje = ""
           wLinje = FILL(wSep,NUM-ENTRIES(wFields,wSep) - 1)
           wHit   = FALSE.
    DO ii = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN hField = hBuffer:BUFFER-FIELD(ii).
        IF hField:NAME = "PerLinNr" AND hField:BUFFER-VALUE() > 999999 THEN DO:
            IF wTot AND hField:BUFFER-VALUE() > 1000000 THEN 
                ASSIGN wLnNr = 2. /* ger färglagd rad */
            ELSE IF NOT wTot THEN
                ASSIGN wLnNr = 2. 
        END.
        IF LOOKUP(hField:NAME,wFields,wSep) = 0 THEN NEXT.
        ASSIGN wHit = TRUE
               ENTRY(LOOKUP(hField:NAME,wFields,wSep),wLinje,wSep) = 
                  REPLACE(STRING(hField:BUFFER-VALUE(),ENTRY(LOOKUP(hField:NAME,wFields,wSep),wFormat,wSep))," ","").
    END.
    IF wHit THEN
       IF wDokTyp = "EX" THEN
          PUT {&UtStream} Unformatted IF wSep <> ";" THEN REPLACE(wLinje,wSep,";") ELSE wLinje SKIP.
       ELSE
          PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
END.
IF wDokTyp = "HTM" THEN
  PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (20)
      HTML;END     ().





