DEFINE INPUT  PARAMETER cTabell   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cQry      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cFileName  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cLabels    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER wFields    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER hQuery     AS HANDLE NO-UNDO.
DEF VAR wLinje  AS CHAR NO-UNDO.
DEF VAR wLnNr   AS INT  NO-UNDO.
DEF VAR wHit    AS LOGI NO-UNDO.
DEF VAR iCount  AS INTE NO-UNDO.
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
/* DEF VAR hQuery  AS HANDLE NO-UNDO. */
IF VALID-HANDLE(hQuery) THEN DO:
    ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:REPOSITION-TO-ROW(1).
END.
ELSE DO:
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE cTabell.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(cQry).
    hQuery:QUERY-OPEN().
END.
OUTPUT TO VALUE(cFileName).
PUT CONTROL "|" REPLACE(cLabels,",","|").
ASSIGN wLnNr = 0.
REPEAT:
    IF wLnNr = 0 THEN
        hQuery:GET-FIRST() NO-ERROR.
    ELSE
        hQuery:GET-NEXT() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        LEAVE.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    Assign wLnNr  = wLnNr + 1
           wLinje = FILL("|",NUM-ENTRIES(wFields) - 1)
           wHit   = FALSE.
    DO iCount = 1 TO NUM-ENTRIES(wFields):
        ASSIGN hField = hBuffer:BUFFER-FIELD(ENTRY(iCount,wFields))
               wHit = TRUE
            ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = 
                IF hField:BUFFER-VALUE = ? THEN "" ELSE IF 
                    hField:DATA-TYPE = "DECIMAL" THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),2),"->>>>>>>>>9.99"))
/*                    hField:DATA-TYPE = "DECIMAL" AND hField:DECIMALS = 0 THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),0),">>>>>>>>>9"))           */
/*                    ELSE IF hField:DATA-TYPE = "DECIMAL" AND hField:DECIMALS = 1 THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),1),">>>>>>>>>9.9")) */
/*                    ELSE IF hField:DATA-TYPE = "DECIMAL" THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),2),">>>>>>>>>9.99"))                        */
                   ELSE IF hField:DATA-TYPE = "CHARACTER" THEN hField:BUFFER-VALUE() ELSE TRIM(REPLACE(hField:STRING-VALUE(),".","")).
/*         ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = TRIM(REPLACE(hField:STRING-VALUE(),".","")). */
/*                ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = TRIM(hField:STRING-VALUE()). */
    END.
    /*
    DO iCount = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN hField = hBuffer:BUFFER-FIELD(iCount).
        IF LOOKUP(hField:NAME,wFields) > 0 THEN
           ASSIGN wHit = TRUE
                  ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = TRIM(STRING(hField:BUFFER-VALUE())).
    END.
    */
    IF wHit THEN DO:
        PUT CONTROL CHR(10) STRING(wLnNr) "|" wLinje.
    END.
END.
OUTPUT CLOSE.




