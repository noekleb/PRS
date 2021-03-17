DEFINE OUTPUT PARAMETER dHapHourId LIKE HappyHourHode.HapHourId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE HappyHourHode.HapHourId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE HappyHourHode.HapHourId NO-UNDO.
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
        FIND LAST HappyHourHode WHERE HappyHourHode.HapHourId >= dFraNr AND
                              HappyHourHode.HapHourId <= dTilNr USE-INDEX HappyHourHode NO-LOCK NO-ERROR.
        IF NOT AVAIL HappyHourHode OR (AVAIL HappyHourHode AND HappyHourHode.HapHourId < dTilNr) THEN DO:
            ASSIGN dHapHourId = IF AVAIL HappyHourHode THEN HappyHourHode.HapHourId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 1000 + 1
               dTilNr = Butiker.Butik * 1000 + 999.
        FIND LAST HappyHourHode WHERE HappyHourHode.HapHourId >= dFraNr AND
                              HappyHourHode.HapHourId <= dTilNr USE-INDEX HappyHourHode NO-LOCK NO-ERROR.

        IF NOT AVAIL HappyHourHode THEN
        DO:
            ASSIGN
                dHapHourId = dFraNr
                .
            LEAVE BUTIKKLOOP.
        END.
        ELSE DO:
            ASSIGN
                dHapHourId = HappyHourHode.HapHourId + 1
                .
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
