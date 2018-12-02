DEFINE OUTPUT PARAMETER dBestNr LIKE SentralDB.BestHode.BestNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL               NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE SentralDB.BestHode.BestNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE SentralDB.BestHode.BestNr NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
FIND SentralDB.SysPara NO-LOCK where
    SentralDB.SysPara.SysHId = 1 and
    SentralDB.SysPara.SysGr  = 1 and
    SentralDB.SysPara.ParaNr = 18 NO-ERROR.
  if AVAILABLE SentralDB.SysPara then
    ASSIGN strLokaltHk = (SentralDB.SysPara.Parameter1).
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
DO:
    FIND SentralDB.SysPara NO-LOCK where
        SentralDB.SysPara.SysHId = 1 and
        SentralDB.SysPara.SysGr  = 1 and
        SentralDB.SysPara.ParaNr = 23 NO-ERROR.
      if AVAILABLE SentralDB.SysPara then
        ASSIGN cHkNumSerier = (SentralDB.SysPara.Parameter1).
END.

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST SentralDB.BestHode WHERE SentralDB.BestHode.BestNr >= dFraNr AND
                              SentralDB.BestHode.BestNr <= dTilNr USE-INDEX BestNr NO-LOCK NO-ERROR.
        IF NOT AVAIL SentralDB.BestHode OR (AVAIL SentralDB.BestHode AND SentralDB.BestHode.BestNr < dTilNr) THEN DO:
            ASSIGN dBestNr = IF AVAIL SentralDB.BestHode THEN SentralDB.BestHode.BestNr + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH SentralDB.Butiker NO-LOCK:
        ASSIGN dFraNr = SentralDB.Butiker.Butik * 100000 + 1
               dTilNr = SentralDB.Butiker.Butik * 100000 + 99999.
        FIND LAST SentralDB.BestHode WHERE SentralDB.BestHode.BestNr >= dFraNr AND
                              SentralDB.BestHode.BestNr <= dTilNr USE-INDEX BestNr NO-LOCK NO-ERROR.

        IF NOT AVAIL SentralDB.BestHode OR (AVAIL SentralDB.BestHode AND SentralDB.BestHode.BestNr < dTilNr) THEN DO:
            ASSIGN dBestNr = IF AVAIL SentralDB.BestHode THEN SentralDB.BestHode.BestNr + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
