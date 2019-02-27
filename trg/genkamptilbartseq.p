DEFINE INPUT  PARAMETER iButikkNr   AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER strLokaltHk AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER iKampTilbArtSeq LIKE KampanjeTilbArtikkel.KampTilbArtSeq  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE KampanjeTilbArtikkel.KampTilbArtSeq NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE KampanjeTilbArtikkel.KampTilbArtSeq NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* KampTilbArtSeq mha systemparameter  */
/*{syspara.i 17 10 1 strLokaltHk}*/

ASSIGN lLokaltHk = can-do("yes,Ja,1",strLokaltHk).
IF lLokaltHk THEN
   {syspara.i 1 1 21 cHkNumSerier}

IF lLokaltHK THEN 
HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST KampanjeTilbArtikkel WHERE KampanjeTilbArtikkel.KampTilbArtSeq >= dFraNr AND
                               KampanjeTilbArtikkel.KampTilbArtSeq <= dTilNr USE-INDEX KampTilbArtSeq NO-LOCK NO-ERROR.
        IF NOT AVAIL KampanjeTilbArtikkel THEN 
        DO:
            ASSIGN iKampTilbArtSeq = dFraNr.
            LEAVE HKLOOP.
        END.
        IF (AVAIL KampanjeTilbArtikkel AND KampanjeTilbArtikkel.KampTilbArtSeq < dTilNr) THEN 
        DO:
            ASSIGN iKampTilbArtSeq = KampanjeTilbArtikkel.KampTilbArtSeq + 1.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr: /* Skal ikke hente nummerserie fra kommisjonsbutikker. */
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST KampanjeTilbArtikkel WHERE 
            KampanjeTilbArtikkel.KampTilbArtSeq >= dFraNr AND
            KampanjeTilbArtikkel.KampTilbArtSeq <= dTilNr USE-INDEX KampTilbArtSeq 
            NO-LOCK NO-ERROR.

        IF NOT AVAIL KampanjeTilbArtikkel OR 
           (AVAIL KampanjeTilbArtikkel AND KampanjeTilbArtikkel.KampTilbArtSeq < dTilNr) THEN 
        DO:
            ASSIGN 
                iKampTilbArtSeq = IF AVAIL KampanjeTilbArtikkel 
                                 THEN KampanjeTilbArtikkel.KampTilbArtSeq + 1 
                                 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.

    END.
END.
