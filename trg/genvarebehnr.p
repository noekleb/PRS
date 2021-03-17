DEFINE OUTPUT PARAMETER dVareBehNr LIKE VareBehHode.VareBehNr NO-UNDO.
DEFINE OUTPUT PARAMETER iButikkNr  LIKE Butiker.butik         NO-UNDO.

DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE VareBehHode.VareBehNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE VareBehHode.VareBehNr NO-UNDO.
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
        FIND LAST VareBehHode WHERE VareBehHode.VareBehNr >= dFraNr AND
                              VareBehHode.VareBehNr <= dTilNr USE-INDEX VareBehHode NO-LOCK NO-ERROR.
        IF NOT AVAIL VareBehHode OR (AVAIL VareBehHode AND VareBehHode.VareBehNr < dTilNr) THEN DO:
            ASSIGN dVareBehNr = IF AVAIL VareBehHode THEN VareBehHode.VareBehNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 10000000.00 + 1.00
               dTilNr = Butiker.Butik * 10000000.00 + 9999999.00.
        FIND LAST VareBehHode WHERE VareBehHode.VareBehNr >= dFraNr AND
                              VareBehHode.VareBehNr <= dTilNr USE-INDEX VareBehHode NO-LOCK NO-ERROR.

        IF NOT AVAIL VareBehHode OR (AVAIL VareBehHode AND VareBehHode.VareBehNr < dTilNr) THEN DO:
            ASSIGN dVareBehNr = IF AVAIL VareBehHode THEN VareBehHode.VareBehNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
