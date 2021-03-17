DEFINE OUTPUT PARAMETER dInterntKKortId LIKE KundeKort.InterntKKortId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE KundeKort.InterntKKortId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE KundeKort.InterntKKortId NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 23 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST KundeKort WHERE KundeKort.InterntKKortId >= dFraNr AND
                                  KundeKort.InterntKKortId <= dTilNr USE-INDEX InterntKKortId NO-LOCK NO-ERROR.
        IF NOT AVAIL Kundekort OR (AVAIL Kundekort AND KundeKort.InterntKKortId < dTilNr) THEN DO:
            ASSIGN dInterntKKortId = IF AVAIL Kundekort THEN KundeKort.InterntKKortId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST KundeKort WHERE Kundekort.InterntKKortId >= dFraNr AND
                                  KundeKort.InterntKKortId <= dTilNr USE-INDEX InterntKKortId NO-LOCK NO-ERROR.

        IF NOT AVAIL KundeKort OR (AVAIL KundeKort AND KundeKort.InterntKKortId < dTilNr) THEN DO:
            ASSIGN dInterntKKortId = IF AVAIL Kundekort THEN KundeKort.InterntKKortId + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
