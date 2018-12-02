DEFINE OUTPUT PARAMETER dKupongId LIKE Kupong.KupongId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Kupong.KupongId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Kupong.KupongId NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 17 10 1 strLokaltHk}
ASSIGN lLokaltHk = can-do("yes,Ja,1",strLokaltHk).

IF lLokaltHk THEN
   {syspara.i 1 1 21 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST Kupong WHERE Kupong.KupongId >= dFraNr AND
                              Kupong.KupongId <= dTilNr USE-INDEX Kupong NO-LOCK NO-ERROR.
        IF NOT AVAIL Kupong OR (AVAIL Kupong AND Kupong.KupongId < dTilNr) THEN DO:
            ASSIGN dKupongId = IF AVAIL Kupong THEN Kupong.KupongId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST Kupong WHERE Kupong.KupongId >= dFraNr AND
                              Kupong.KupongId <= dTilNr USE-INDEX Kupong NO-LOCK NO-ERROR.

        IF NOT AVAIL Kupong THEN
        DO:
            ASSIGN
                dKupongId = dFraNr
                .
            LEAVE BUTIKKLOOP.
        END.
        ELSE DO:
            ASSIGN
                dKupongId = Kupong.KupongId + 1
                .
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
