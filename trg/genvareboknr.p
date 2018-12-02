DEFINE OUTPUT PARAMETER dVareBokNr LIKE VareBokHode.VareBokNr NO-UNDO.
DEFINE OUTPUT PARAMETER iButikkNr  LIKE Butiker.butik         NO-UNDO.

DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE VareBokHode.VareBokNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE VareBokHode.VareBokNr NO-UNDO.
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
        FIND LAST VareBokHode WHERE VareBokHode.VareBokNr >= dFraNr AND
                              VareBokHode.VareBokNr <= dTilNr USE-INDEX VareBokHode NO-LOCK NO-ERROR.
        IF NOT AVAIL VareBokHode OR (AVAIL VareBokHode AND VareBokHode.VareBokNr < dTilNr) THEN DO:
            ASSIGN dVareBokNr = IF AVAIL VareBokHode THEN VareBokHode.VareBokNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = dec(string(Butiker.Butik,"999999") + "0000001")
               dTilNr = dec(string(Butiker.Butik,"999999") + "9999999").
        FIND LAST VareBokHode WHERE VareBokHode.VareBokNr >= dFraNr AND
                              VareBokHode.VareBokNr <= dTilNr USE-INDEX VareBokHode NO-LOCK NO-ERROR.

        IF NOT AVAIL VareBokHode OR (AVAIL VareBokHode AND VareBokHode.VareBokNr < dTilNr) THEN 
        DO:
            ASSIGN dVareBokNr = IF AVAIL VareBokHode THEN VareBokHode.VareBokNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
