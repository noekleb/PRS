DEFINE OUTPUT PARAMETER dLoggNr    LIKE PrintLogg.LoggNr NO-UNDO.
DEFINE OUTPUT PARAMETER iButikkNr  LIKE Butiker.butik         NO-UNDO.

DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE PrintLogg.LoggNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE PrintLogg.LoggNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 23 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST PrintLogg WHERE PrintLogg.LoggNr >= dFraNr AND
                              PrintLogg.LoggNr <= dTilNr NO-LOCK NO-ERROR.
        IF NOT AVAIL PrintLogg OR (AVAIL PrintLogg AND PrintLogg.LoggNr < dTilNr) THEN DO:
            ASSIGN dLoggNr = IF AVAIL PrintLogg THEN PrintLogg.LoggNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = dec(string(Butiker.Butik,"999999") + "0000001")
               dTilNr = dec(string(Butiker.Butik,"999999") + "9999999").
        FIND LAST PrintLogg WHERE PrintLogg.LoggNr >= dFraNr AND
                              PrintLogg.LoggNr <= dTilNr NO-LOCK NO-ERROR.

        IF NOT AVAIL PrintLogg OR (AVAIL PrintLogg AND PrintLogg.LoggNr < dTilNr) THEN 
        DO:
            ASSIGN dLoggNr = IF AVAIL PrintLogg THEN PrintLogg.LoggNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
