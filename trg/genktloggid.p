DEFINE OUTPUT PARAMETER dKTLoggId LIKE KupongTransLogg.KTLoggId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE KupongTransLogg.KTLoggId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE KupongTransLogg.KTLoggId NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 24 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST KupongTransLogg WHERE KupongTransLogg.KTLoggId >= dFraNr AND
                              KupongTransLogg.KTLoggId <= dTilNr USE-INDEX KupongTransLogg NO-LOCK NO-ERROR.
        IF NOT AVAIL KupongTransLogg OR (AVAIL KupongTransLogg AND KupongTransLogg.KTLoggId < dTilNr) THEN DO:
            ASSIGN dKTLoggId = IF AVAIL KupongTransLogg THEN KupongTransLogg.KTLoggId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 1000 + 1
               dTilNr = Butiker.Butik * 1000 + 999.
        FIND LAST KupongTransLogg WHERE KupongTransLogg.KTLoggId >= dFraNr AND
                              KupongTransLogg.KTLoggId <= dTilNr USE-INDEX KupongTransLogg NO-LOCK NO-ERROR.

        IF NOT AVAIL KupongTransLogg THEN
        DO:
            ASSIGN
                dKTLoggId = dFraNr
                .
            LEAVE BUTIKKLOOP.
        END.
        ELSE DO:
            ASSIGN
                dKTLoggId = KupongTransLogg.KTLoggId + 1
                .
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
