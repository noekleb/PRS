DEFINE OUTPUT PARAMETER dProdFamId LIKE ProduktFamilie.ProdFamId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE ProduktFamilie.ProdFamId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE ProduktFamilie.ProdFamId NO-UNDO.
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
        FIND LAST ProduktFamilie WHERE ProduktFamilie.ProdFamId >= dFraNr AND
                              ProduktFamilie.ProdFamId <= dTilNr USE-INDEX ProduktFamilie NO-LOCK NO-ERROR.
        IF NOT AVAIL ProduktFamilie OR (AVAIL ProduktFamilie AND ProduktFamilie.ProdFamId < dTilNr) THEN DO:
            ASSIGN dProdFamId = IF AVAIL ProduktFamilie THEN ProduktFamilie.ProdFamId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 1000 + 1
               dTilNr = Butiker.Butik * 1000 + 999.
        FIND LAST ProduktFamilie WHERE ProduktFamilie.ProdFamId >= dFraNr AND
                              ProduktFamilie.ProdFamId <= dTilNr USE-INDEX ProduktFamilie NO-LOCK NO-ERROR.

        IF NOT AVAIL ProduktFamilie THEN
        DO:
            ASSIGN
                dProdFamId = dFraNr
                .
            LEAVE BUTIKKLOOP.
        END.
        ELSE DO:
            ASSIGN
                dProdFamId = ProduktFamilie.ProdFamId + 1
                .
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
