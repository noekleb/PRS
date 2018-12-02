DEFINE OUTPUT PARAMETER dPkSdlId LIKE PkSdlHode.PkSdlId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       AS DEC NO-UNDO.
DEFINE VARIABLE dTilNr       AS DEC NO-UNDO.
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
        FIND LAST PkSdlHode WHERE PkSdlHode.PkSdlId >= dFraNr AND
                                  PkSdlHode.PkSdlId <= dTilNr USE-INDEX PkSdlId NO-LOCK NO-ERROR.
        IF NOT AVAIL PkSdlHode OR (AVAIL PkSdlHode AND PkSdlHode.PkSdlId < dTilNr) THEN DO:
            ASSIGN dPkSdlId = IF AVAIL PkSdlHode THEN PkSdlHode.PkSdlId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST PkSdlHode WHERE PkSdlHode.PkSdlId >= dFraNr AND
                                  PkSdlHode.PkSdlId <= dTilNr USE-INDEX PkSdlId NO-LOCK NO-ERROR.

        IF NOT AVAIL PkSdlHode OR (AVAIL PkSdlHode AND PkSdlHode.PkSdlId < dTilNr) THEN DO:
            ASSIGN dPkSdlId = IF AVAIL PkSdlHode THEN PkSdlHode.PkSdlId + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
