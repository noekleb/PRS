DEFINE OUTPUT PARAMETER dVPIMottakId LIKE VPIMottak.VPIMottakId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE Ordre.OrdreNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE Ordre.OrdreNr NO-UNDO.
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
        FIND LAST VPIMottak WHERE VPIMottak.VPIMottakId >= dFraNr AND
                              VPIMottak.VPIMottakId <= dTilNr USE-INDEX VPIMottakId NO-LOCK NO-ERROR.
        IF NOT AVAIL VPIMottak OR (AVAIL VPIMottak AND VPIMottak.VPIMottakId < dTilNr) THEN DO:
            ASSIGN dVPIMottakId = IF AVAIL VPIMottak THEN VPIMottak.VPIMottakId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST VPIMottak WHERE VPIMottak.VPIMottakId >= dFraNr AND
                              VPIMottak.VPIMottakId <= dTilNr USE-INDEX VPIMottakId NO-LOCK NO-ERROR.
        IF NOT AVAIL VPIMottak OR (AVAIL VPIMottak AND VPIMottak.VPIMottakId < dTilNr) THEN DO:
            ASSIGN dVPIMottakId = IF AVAIL VPIMottak THEN VPIMottak.VPIMottakId + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
