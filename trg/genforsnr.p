DEFINE OUTPUT PARAMETER dForsNr LIKE Forsalj.ForsNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Forsalj.ForsNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Forsalj.ForsNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 24 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST Forsalj WHERE Forsalj.ForsNr >= dFraNr AND
                              Forsalj.ForsNr <= dTilNr USE-INDEX Forsalj NO-LOCK NO-ERROR.
        IF NOT AVAIL Forsalj OR (AVAIL Forsalj AND Forsalj.ForsNr < dTilNr) THEN DO:
            ASSIGN dForsNr = IF AVAIL Forsalj THEN Forsalj.ForsNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 1000 + 1
               dTilNr = Butiker.Butik * 1000 + 999.
        FIND LAST Forsalj WHERE Forsalj.ForsNr >= dFraNr AND
                              Forsalj.ForsNr <= dTilNr USE-INDEX forsalin NO-LOCK NO-ERROR.

        IF NOT AVAIL Forsalj THEN
        DO:
            ASSIGN
                dForsNr = dFraNr
                .
            LEAVE BUTIKKLOOP.
        END.
        ELSE DO:
            ASSIGN
                dForsNr = Forsalj.ForsNr + 1
                .
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
