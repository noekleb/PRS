DEFINE OUTPUT PARAMETER dKundeNr LIKE Kunde.KundeNr     NO-UNDO.
DEFINE OUTPUT PARAMETER iButikkNr LIKE Butiker.butik  NO-UNDO.
DEFINE OUTPUT PARAMETER cKortNr LIKE KundeKort.KortNr NO-UNDO.

DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Kunde.KundeNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Kunde.KundeNr NO-UNDO.
DEFINE VARIABLE iLoop        AS INTEGER NO-UNDO.

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
        FIND LAST Kunde WHERE Kunde.KundeNr >= dFraNr AND
                              Kunde.KundeNr <= dTilNr USE-INDEX Kunde NO-LOCK NO-ERROR.
        IF NOT AVAIL Kunde OR (AVAIL Kunde AND Kunde.KundeNr < dTilNr) THEN DO:
            ASSIGN dKundeNr = IF AVAIL Kunde THEN Kunde.KundeNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST Kunde WHERE Kunde.KundeNr >= dFraNr AND
                              Kunde.KundeNr <= dTilNr USE-INDEX Kunde NO-LOCK NO-ERROR.

        IF NOT AVAIL Kunde OR (AVAIL Kunde AND Kunde.KundeNr < dTilNr) THEN DO:
            ASSIGN dKundeNr = IF AVAIL Kunde THEN Kunde.KundeNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
IF dKundeNr > 0 THEN DO:
/*    {syspara.i 5 1 1 iButikkNr INT}                                                 */
/*    ASSIGN cKortNr = SUBSTR(STRING(dKundeNr),LENGTH(STRING(dKundeNr)) - 5) NO-ERROR.*/
/*    REPEAT:                                                                         */
/*        RUN sjekkomkorterunikt.p (INPUT cKortNr).                                   */
/*        IF RETURN-VALUE = "" THEN                                                   */
/*            LEAVE.                                                                  */
/*        IF cKortNr = SUBSTR(STRING(dKundeNr),LENGTH(STRING(dKundeNr)) - 5) THEN     */
/*            ASSIGN cKortNr = STRING(DECI(cKortNr) + 5001).                          */
/*        ELSE                                                                        */
/*            ASSIGN cKortNr = STRING(DECI(cKortnr) + 1).                             */
/*    END.                                                                            */
    
    ASSIGN 
        cKortNr = STRING(dKundeNr) 
        iLoop   = 0 NO-ERROR.
    REPEAT:
        iLoop = iLoop + 1.
        RUN sjekkomkorterunikt.p (INPUT cKortNr).
        IF RETURN-VALUE = "" THEN
            LEAVE.
        ELSE 
            ASSIGN cKortNr = STRING(dKundeNr) + STRING(iLoop).        
    END.
    
END.
