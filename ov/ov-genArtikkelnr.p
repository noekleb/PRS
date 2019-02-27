DEFINE OUTPUT PARAMETER dArtikkelNr LIKE SentralDB.ArtBas.ArtikkelNr  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE SentralDB.ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE SentralDB.ArtBas.ArtikkelNr NO-UNDO.
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
        SentralDB.SysPara.ParaNr = 21 NO-ERROR.
      if AVAILABLE SentralDB.SysPara then
        ASSIGN cHkNumSerier = (SentralDB.SysPara.Parameter1).
END.

IF lLokaltHK THEN 
HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST SentralDB.ArtBas WHERE SentralDB.ArtBas.ArtikkelNr >= dFraNr AND
                               SentralDB.ArtBas.ArtikkelNr <= dTilNr USE-INDEX Artikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAIL SentralDB.ArtBas THEN 
        DO:
            ASSIGN dArtikkelNr = dFraNr.
            LEAVE HKLOOP.
        END.
        IF (AVAIL SentralDB.ArtBas AND SentralDB.ArtBas.ArtikkelNr < dTilNr) THEN 
        DO:
            ASSIGN dArtikkelNr = SentralDB.ArtBas.ArtikkelNr + 1.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH SentralDB.Butiker NO-LOCK WHERE
        SentralDB.Butiker.Butik <= 999: /* Skal ikke hente nummerserie fra kommisjonsbutikker. */
        ASSIGN dFraNr = SentralDB.Butiker.Butik * 10000 + 1
               dTilNr = SentralDB.Butiker.Butik * 10000 + 9999.
        FIND LAST SentralDB.ArtBas WHERE 
            SentralDB.ArtBas.ArtikkelNr >= dFraNr AND
            SentralDB.ArtBas.ArtikkelNr <= dTilNr USE-INDEX Artikkelnr 
            NO-LOCK NO-ERROR.

        IF NOT AVAIL SentralDB.ArtBas OR 
           (AVAIL SentralDB.ArtBas AND SentralDB.ArtBas.ArtikkelNr < dTilNr) THEN 
        DO:
            ASSIGN 
                dArtikkelNr = IF AVAIL SentralDB.ArtBas 
                                THEN SentralDB.ArtBas.ArtikkelNr + 1 
                                ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.


