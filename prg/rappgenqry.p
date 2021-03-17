DEFINE INPUT PARAMETER cTabell    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cQry       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cFileName  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cLabels    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER wFields    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cDecimaler AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER cTidFelter AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER hQuery     AS HANDLE NO-UNDO.
DEFINE       VARIABLE  cLabel2    AS CHARACTER  NO-UNDO.

DEF VAR lCreate AS LOGICAL    NO-UNDO.
DEF VAR wLinje  AS CHAR NO-UNDO.
DEF VAR wLnNr   AS INT  NO-UNDO.
DEF VAR wHit    AS LOGI NO-UNDO.
DEF VAR iCount  AS INTE NO-UNDO.
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE iAntDeci AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSELabels AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSELabel2 AS CHARACTER   NO-UNDO.

/* DEF VAR hQuery  AS HANDLE NO-UNDO. */
IF VALID-HANDLE(hQuery) THEN DO:
    ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE(1).
/*     hQuery:QUERY-OPEN(). */
    hQuery:REPOSITION-TO-ROW(1).
END.
ELSE DO:
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE cTabell.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(cQry).
    hQuery:QUERY-OPEN().
    lCreate = TRUE.
END.

IF NUM-ENTRIES(wFields) <> NUM-ENTRIES(cDecimaler) THEN DO:
    MESSAGE "Fel i decimalsträng, inga decimaler skrivs ut."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN cDecimaler =  FILL(",",NUM-ENTRIES(wFields) - 1).
END.
IF NUM-ENTRIES(cLabels,CHR(1)) = 2 THEN
    ASSIGN cLabel2 = ENTRY(2,cLabels,CHR(1))
           cLabels = ENTRY(1,cLabels,CHR(1)).
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
/* IF AVAIL bruker AND CAN-DO("SE,SVE",bruker.Lng) THEN DO:                */
/*     RUN Oversett2SE(cLabels,cLabel2,OUTPUT cSELabels,OUTPUT cSELabel2). */
/* END.                                                                    */
/* OUTPUT TO VALUE(SESSION:TEMP-DIR + "\" + "LABELS.txt") APPEND. */
cLabels = IF DYNAMIC-FUNCTION('Oversett2SE', INPUT cLabels) <> ? THEN DYNAMIC-FUNCTION('Oversett2SE', INPUT cLabels) ELSE cLabels NO-ERROR.
/* PUT UNFORMATTED cLabels SKIP. */
IF cLabel2 <> "" THEN DO:
    cLabel2 = IF DYNAMIC-FUNCTION('Oversett2SE', INPUT cLabel2) <> ? THEN DYNAMIC-FUNCTION('Oversett2SE', INPUT cLabel2) ELSE cLabel2 NO-ERROR.
/*     PUT UNFORMATTED cLabel2 SKIP. */
END.
/* OUTPUT CLOSE. */

OUTPUT TO VALUE(cFileName).
PUT CONTROL "|" REPLACE(cLabels,",","|").
IF cLabel2 <> "" THEN
    PUT CONTROL CHR(10) "|" REPLACE(cLabel2,",","|").

ASSIGN wLnNr = 0.
REPEAT:
    IF wLnNr = 0 THEN
        hQuery:GET-FIRST() NO-ERROR.
    ELSE
        hQuery:GET-NEXT() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        LEAVE.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    ASSIGN wLnNr  = wLnNr + 1
        wLinje = FILL(CHR(1),NUM-ENTRIES(wFields) - 1)
/*            wLinje = FILL("|",NUM-ENTRIES(wFields) - 1) */
           wHit   = FALSE.
    DO iCount = 1 TO NUM-ENTRIES(wFields):
        ASSIGN hField = hBuffer:BUFFER-FIELD(ENTRY(iCount,wFields))
               wHit = TRUE
               iAntDeci = INT(ENTRY(iCount,cDecimaler))
/*             ENTRY(LOOKUP(hField:NAME,wFields),wLinje,"|") = */
            ENTRY(LOOKUP(hField:NAME,wFields),wLinje,CHR(1)) = 
                IF hField:BUFFER-VALUE = ? THEN "" ELSE IF 
                    CAN-DO(cTidFelter,hField:NAME) THEN STRING(hField:BUFFER-VALUE(),"HH:MM:SS")
                    ELSE IF 
                        /*                     hField:DATA-TYPE = "DECIMAL" THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),2),"->>>>>>>>>9.99")) */
                   hField:NAME = "b_id" AND hField:DATA-TYPE = "DECIMAL" THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),0),">>>>>>>>>>>>>>>>>>>>>9"))
                   ELSE if hField:DATA-TYPE = "DECIMAL" AND iAntDeci = 0 THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),0),"->>>>>>>>>>>>9"))
                   ELSE IF hField:DATA-TYPE = "DECIMAL" AND iAntDeci = 1 THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),1),"->>>>>>>>>>>9.9"))
                   ELSE IF hField:DATA-TYPE = "DECIMAL" AND iAntDeci = 3 THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),3),"->>>>>>>>>>9.999"))
                   ELSE IF hField:DATA-TYPE = "DECIMAL" THEN TRIM(STRING(ROUND(hField:BUFFER-VALUE(),2),"->>>>>>>>>>9.99"))
                   ELSE IF hField:DATA-TYPE = "LOGICAL" THEN STRING(hField:BUFFER-VALUE(),"J/N")
                   ELSE IF hField:DATA-TYPE = "CHARACTER" AND hField:NAME = "DataObjekt" THEN LEFT-TRIM(hField:BUFFER-VALUE(),"0")
                   ELSE IF hField:DATA-TYPE = "CHARACTER" THEN REPLACE(REPLACE(hField:BUFFER-VALUE(),CHR(10),""),CHR(13),"")
/*                    ELSE IF hField:DATA-TYPE = "CHARACTER" THEN hField:BUFFER-VALUE() */
                   ELSE IF hField:DATA-TYPE = "DATE" THEN SUBSTR(STRING(YEAR(DATE(hField:STRING-VALUE()))),3) + "/" + STRING(MONTH(DATE(hField:STRING-VALUE())),"99") + "/" + STRING(DAY(DATE(hField:STRING-VALUE())),"99")
                   ELSE IF hField:DATA-TYPE = "INTEGER" THEN TRIM(STRING(hField:BUFFER-VALUE(),"->>>>>>>>9")) ELSE
                       TRIM(REPLACE(hField:STRING-VALUE(),".","")).
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
        ASSIGN wLinje = REPLACE(wLinje,'"','')
               wLinje = REPLACE(wLinje,'|','ø')
               wLinje = REPLACE(wLinje,CHR(3),'|')
               wLinje = REPLACE(wLinje,CHR(2),'|')
               wLinje = REPLACE(wLinje,CHR(1),'|').
        PUT CONTROL CHR(10) STRING(wLnNr) "|" TRIM(wLinje).
/*         PUT CONTROL CHR(10) STRING(wLnNr) "|" wLinje. */
    END.
END.
OUTPUT CLOSE.
hQuery:QUERY-CLOSE().
IF lCreate = TRUE THEN DO:
    DELETE OBJECT hBuffer NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    hBuffer = ?.
    hQuery  = ?.
END.
