DEFINE OUTPUT PARAMETER dMedlemsNr LIKE Medlem.MedlemsNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Medlem.MedlemsNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Medlem.MedlemsNr NO-UNDO.
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
        FIND LAST Medlem WHERE Medlem.MedlemsNr >= dFraNr AND
                              Medlem.MedlemsNr <= dTilNr USE-INDEX Medlem NO-LOCK NO-ERROR.
        IF NOT AVAIL Medlem OR (AVAIL Medlem AND Medlem.MedlemsNr < dTilNr) THEN DO:
            ASSIGN dMedlemsNr = IF AVAIL Medlem THEN Medlem.MedlemsNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST Medlem WHERE Medlem.MedlemsNr >= dFraNr AND
                              Medlem.MedlemsNr <= dTilNr USE-INDEX Medlem NO-LOCK NO-ERROR.

        IF NOT AVAIL Medlem OR (AVAIL Medlem AND Medlem.MedlemsNr < dTilNr) THEN DO:
            ASSIGN dMedlemsNr = IF AVAIL Medlem THEN Medlem.MedlemsNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
