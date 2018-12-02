DEFINE OUTPUT PARAMETER dOrdreNr LIKE Ordre.OrdreNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Ordre.OrdreNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Ordre.OrdreNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 23 cHkNumSerier}

/* HAr vi butikker med flere enn 3 siffer i butikknr får vi en utfording med integerfelt */
FIND LAST Ordre NO-LOCK USE-INDEX OrdreNr NO-ERROR.
IF AVAILABLE Ordre THEN
    dOrdreNr = Ordre.OrdreNr + 1.
ELSE
    dOrdreNr = 1.

/*
IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST Ordre WHERE Ordre.OrdreNr >= dFraNr AND
                              Ordre.OrdreNr <= dTilNr USE-INDEX OrdreNr NO-LOCK NO-ERROR.
        IF NOT AVAIL Ordre OR (AVAIL Ordre AND Ordre.OrdreNr < dTilNr) THEN DO:
            ASSIGN dOrdreNr = IF AVAIL Ordre THEN Ordre.OrdreNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST Ordre WHERE Ordre.OrdreNr >= dFraNr AND
                              Ordre.OrdreNr <= dTilNr USE-INDEX OrdreNr NO-LOCK NO-ERROR.

        IF NOT AVAIL Ordre OR (AVAIL Ordre AND Ordre.OrdreNr < dTilNr) THEN DO:
            ASSIGN dOrdreNr = IF AVAIL Ordre THEN Ordre.OrdreNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
*/
