DEFINE OUTPUT PARAMETER dReklamasjonsNr LIKE Reklamasjonslogg.ReklamasjonsNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL               NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Reklamasjonslogg.ReklamasjonsNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Reklamasjonslogg.ReklamasjonsNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
DO:
    {syspara.i 1 1 27 cHkNumSerier}
    IF cHkNumSerier = "" THEN
        cHkNumSerier = "90000001-99999999,85000001-89999999".
END.

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST Reklamasjonslogg WHERE Reklamasjonslogg.ReklamasjonsNr >= dFraNr AND
                              Reklamasjonslogg.ReklamasjonsNr <= dTilNr USE-INDEX ReklamasjonsNr NO-LOCK NO-ERROR.
        IF NOT AVAIL Reklamasjonslogg OR (AVAIL Reklamasjonslogg AND Reklamasjonslogg.ReklamasjonsNr < dTilNr) THEN DO:
            ASSIGN dReklamasjonsNr = IF AVAIL Reklamasjonslogg THEN Reklamasjonslogg.ReklamasjonsNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST Reklamasjonslogg WHERE Reklamasjonslogg.ReklamasjonsNr >= dFraNr AND
                              Reklamasjonslogg.ReklamasjonsNr <= dTilNr USE-INDEX ReklamasjonsNr NO-LOCK NO-ERROR.

        IF NOT AVAIL Reklamasjonslogg OR (AVAIL Reklamasjonslogg AND Reklamasjonslogg.ReklamasjonsNr < dTilNr) THEN DO:
            ASSIGN dReklamasjonsNr = IF AVAIL Reklamasjonslogg THEN Reklamasjonslogg.ReklamasjonsNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
