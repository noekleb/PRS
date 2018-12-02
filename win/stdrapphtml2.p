 /*

	Last change:  TN   23 Nov 99    1:11 pm
*/


&Scoped UtStream Stream Ut
DEF STREAM Ut.

DEF VAR wLinje AS CHAR NO-UNDO.
DEF VAR wLnNr  AS INT  NO-UNDO.
DEF VAR wt     AS CHAR NO-UNDO.
DEF VAR j      AS INT  NO-UNDO.
DEF VAR wNr    AS INT  NO-UNDO.
DEF VAR wRap   AS CHAR NO-UNDO INIT "1,2,3".

DEFINE INPUT PARAMETER wTitle       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wRapport     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead1       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead1Set    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHead     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHeadForm AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTabell      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wQY          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFields      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wDokTyp      AS CHAR NO-UNDO.

DEF VAR wHit AS LOGI NO-UNDO.
DEF VAR ii AS INTE NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.

{tmphtmlrapp.i}

CREATE QUERY hQuery.
DEF VAR hBuffer AS HANDLE NO-UNDO.
/* CREATE BUFFER hBuffer FOR TABLE wTabell. --  Kan ikke brukes p† tmp file. */
hBuffer = BUFFER tmpHtml:HANDLE. /* For h†ndtering av tmp-file */

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
{htmlwrapperdef.i }
IF wDokTyp = "Html" THEN
    Output {&UtStream} to "rapport.htm".
ELSE
    Output {&UtStream} to "rapport.sdv".

IF wDoktyp = "Excel" THEN
  PUT {&UtStream} Unformatted
    REPLACE(wColHead,"|",";") SKIP.   
ELSE
  PUT {&UtStream} Unformatted
    HTML;Start ("|",wTitle,"")
    HTML;Head1 (wHead1,ENTRY(1,wHead1Set),ENTRY(2,wHead1Set),INT(ENTRY(3,wHead1Set)),INT(ENTRY(4,wHead1Set)),INT(ENTRY(5,wHead1Set)),INT(ENTRY(6,wHead1Set)))
    HTML;Head2 (wHead2)
    HTML;ColHead (wColHead,wColHeadForm). 
ASSIGN wLnNr = 0.

hQuery:QUERY-OPEN().
REPEAT:
    hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    Assign wLnNr  = wLnNr + 1
           wLinje = FILL("|",NUM-ENTRIES(wFields) - 1)
           wHit   = FALSE.
    
    DO ii = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN hField = hBuffer:BUFFER-FIELD(ii).
        IF LOOKUP(hField:NAME,wFields) > 0 OR wFields = "" THEN
           ASSIGN wHit = TRUE
                  ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = hField:STRING-VALUE().
    END.
    IF wHit THEN
       IF wDokTyp = "Excel" THEN
          PUT {&UtStream} Unformatted REPLACE(wLinje,"|",";") SKIP.
       ELSE
          PUT {&UtStream} Unformatted HTML;Col(wLinje, "",wLnNr).
END.

IF wDokTyp = "Html" THEN
  PUT {&UtStream} Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (20)
      HTML;END     ().





