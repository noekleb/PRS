DEFINE INPUT  PARAMETER iBilagsType AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER dFakturaNr  AS DEC NO-UNDO.
DEFINE INPUT  PARAMETER iButikkNr   AS INT NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE FakturaHode.FakturaNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE FakturaHode.FakturaNr NO-UNDO.

/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 28 cHkNumSerier}

BUTIKK:
DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iButikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
    DO:
        ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),4,1)) * 100000 + 1
               dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),4,1)) * 100000 + 99999.
        FIND LAST FakturaHode WHERE 
            FakturaHode.BilagsType = iBilagstype AND
            FakturaHode.FakturaNr >= dFraNr AND
            FakturaHode.FakturaNr <= dTilNr USE-INDEX FakturaNr NO-LOCK NO-ERROR.

        IF NOT AVAIL FakturaHode OR 
            (AVAIL FakturaHode AND FakturaHode.FakturaNr < dTilNr) THEN 
        DO:
            ASSIGN dFakturaNr = IF AVAIL FakturaHode 
                                  THEN FakturaHode.FakturaNr + 1 
                                  ELSE dFraNr.
            LEAVE BUTIKK.
        END.
    END.
END. /* BUTIKK */

/*
IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST FakturaHode WHERE 
            FakturaHode.Bilagstype = iBilagstype AND
            FakturaHode.FakturaNr >= dFraNr AND
            FakturaHode.FakturaNr <= dTilNr USE-INDEX FakturaNr NO-LOCK NO-ERROR.
        IF NOT AVAIL FakturaHode OR (AVAIL FakturaHode AND FakturaHode.FakturaNr < dTilNr) THEN DO:
            ASSIGN dFakturaNr = IF AVAIL FakturaHode THEN FakturaHode.FakturaNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),4,1)) * 100000 + 1
               dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),4,1)) * 100000 + 99999.
        FIND LAST FakturaHode WHERE 
            FakturaHode.BilagsType = iBilagstype AND
            FakturaHode.FakturaNr >= dFraNr AND
            FakturaHode.FakturaNr <= dTilNr USE-INDEX FakturaNr NO-LOCK NO-ERROR.

        IF NOT AVAIL FakturaHode OR 
            (AVAIL FakturaHode AND FakturaHode.FakturaNr < dTilNr) THEN 
        DO:
            ASSIGN dFakturaNr = IF AVAIL FakturaHode 
                                  THEN FakturaHode.FakturaNr + 1 
                                  ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
*/
