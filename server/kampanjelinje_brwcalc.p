/* kampanjelinje_brwcalc.p */

DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE cTotalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 150 1 2 iButNr INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iButNr NO-ERROR.

/* **********************  Internal Procedures  *********************** */

PROCEDURE KampanjeLinje_Rab%:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE dDiffKr AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ddiff% AS DECIMAL FORMAT "->>>9.9" NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
        ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN 
      DO:
        dDiffKr = ArtPris.Pris[1] - KampanjeLinje.Pris[2].
        dDiff% = ROUND((dDiffKr / ArtPris.Pris[1]) * 100,1).
        ocValue = IF dDiff% <> ? THEN STRING(dDiff%) ELSE ''.
      END.      
    END.

END PROCEDURE.

PROCEDURE KampanjeLinje_Endret:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
      ASSIGN 
      ocValue = STRING(DATETIME(Kampanjelinje.EDato, Kampanjelinje.ETid * 1000),"99/99/9999 HH:MM:SS") + ' ' + Kampanjelinje.Brukerid
      .

END PROCEDURE.

PROCEDURE KampanjeLinje_Registrert:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
      ASSIGN 
      ocValue = STRING(DATETIME(Kampanjelinje.RegistrertDato, Kampanjelinje.RegistrertTid * 1000),"99/99/9999 HH:MM:SS") + ' ' + Kampanjelinje.RegistrertAv
      .

END PROCEDURE.

PROCEDURE KampanjeLinje_Beskr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = kampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.Beskr.
    END.

END PROCEDURE.

PROCEDURE KampanjeLinje_LevKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = kampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.LevKod.
    END.

END PROCEDURE.

PROCEDURE KampanjeLinje_LevFargKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = kampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.LevFargKod.
    END.

END PROCEDURE.

PROCEDURE KampanjeLinje_Sasong:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = kampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
        FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE Sasong THEN  
        ocValue = Sasong.SasBeskr.
    END.

END PROCEDURE.

PROCEDURE KampanjeLinje_SasongKode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjelinje NO-LOCK
        WHERE ROWID(Kampanjelinje) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjelinje THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = kampanjeLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
        FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE Sasong THEN  
        ocValue = STRING(Sasong.Sasong).
    END.

END PROCEDURE.






