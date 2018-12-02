DEFINE OUTPUT PARAMETER iKampTilbId LIKE KampanjeTilbud.KampTilbId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE KampanjeTilbud.KampTilbId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE KampanjeTilbud.KampTilbId NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* KampTilbId mha systemparameter  */
{syspara.i 17 10 1 strLokaltHk}
ASSIGN lLokaltHk = can-do("yes,Ja,1",strLokaltHk).
IF lLokaltHk THEN
   {syspara.i 1 1 21 cHkNumSerier}

IF lLokaltHK THEN 
HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST KampanjeTilbud WHERE KampanjeTilbud.KampTilbId >= dFraNr AND
                               KampanjeTilbud.KampTilbId <= dTilNr USE-INDEX KampTilbId2 NO-LOCK NO-ERROR.
        IF NOT AVAIL KampanjeTilbud THEN 
        DO:
            ASSIGN iKampTilbId = dFraNr.
            LEAVE HKLOOP.
        END.
        IF (AVAIL KampanjeTilbud AND KampanjeTilbud.KampTilbId < dTilNr) THEN 
        DO:
            ASSIGN iKampTilbId = KampanjeTilbud.KampTilbId + 1.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik <= 999: /* Skal ikke hente nummerserie fra kommisjonsbutikker. */
        ASSIGN dFraNr = Butiker.Butik * 10000 + 1
               dTilNr = Butiker.Butik * 10000 + 9999.
        FIND LAST KampanjeTilbud WHERE 
            KampanjeTilbud.KampTilbId >= dFraNr AND
            KampanjeTilbud.KampTilbId <= dTilNr USE-INDEX KampTilbId2 
            NO-LOCK NO-ERROR.

        IF NOT AVAIL KampanjeTilbud OR 
           (AVAIL KampanjeTilbud AND KampanjeTilbud.KampTilbId < dTilNr) THEN 
        DO:
            ASSIGN 
                iKampTilbId = IF AVAIL KampanjeTilbud 
                                 THEN KampanjeTilbud.KampTilbId + 1 
                                 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.

    END.
END.


