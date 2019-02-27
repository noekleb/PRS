DEFINE OUTPUT PARAMETER dReskontro_Id AS DEC NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Kundereskontr.Reskontro_Id NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Kundereskontr.Reskontro_Id NO-UNDO.
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
        FIND LAST Kundereskontr WHERE Kundereskontr.Reskontro_Id >= dFraNr AND
                              Kundereskontr.Reskontro_Id <= dTilNr USE-INDEX Kundereskontr NO-LOCK NO-ERROR.
        IF NOT AVAIL Kundereskontr OR (AVAIL Kundereskontr AND Kundereskontr.Reskontro_Id < dTilNr) THEN DO:
            ASSIGN dReskontro_Id = IF AVAIL Kundereskontr THEN Kundereskontr.Reskontro_Id + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 1
               dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 9999999.
        FIND LAST Kundereskontr WHERE Kundereskontr.Reskontro_Id >= dFraNr AND
                              Kundereskontr.Reskontro_Id <= dTilNr USE-INDEX Kundereskontr NO-LOCK NO-ERROR.

        IF NOT AVAIL Kundereskontr OR (AVAIL Kundereskontr AND Kundereskontr.Reskontro_Id < dTilNr) THEN DO:
            ASSIGN dReskontro_Id = IF AVAIL Kundereskontr THEN Kundereskontr.Reskontro_Id + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
