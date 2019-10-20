/* kampanjehode_brwcalc.p */

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

PROCEDURE Kampanjehode_AktivTilbud:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode AND KampanjeHode.aktivert THEN
    DO:
      LOOPEN:
      FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK:
        IF CAN-FIND(FIRST ArtPris WHERE 
                    ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
                    ArtPris.ProfilNr   = KampanjeLinje.ProfilNr AND  
                    ArtPris.Tilbud = TRUE AND 
                    ArtPris.TilbudFraDato >= KampanjeHode.StartDato AND 
                    ArtPris.TilbudTilDato <= KampanjeHode.SluttDato) THEN 
          ASSIGN 
            ocValue = '1'
            .
        IF ocValue <> '' THEN 
          LEAVE LOOPEN.
      END. /* LOOPEN */  
    END.

END PROCEDURE.

PROCEDURE Kampanjehode_Endret:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(DATETIME(Kampanjehode.EDato, Kampanjehode.ETid * 1000),"99/99/9999 HH:MM:SS") + ' ' + KampanjeHode.Brukerid
      .

END PROCEDURE.

PROCEDURE fiKamp%:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(Kampanjehode.Kamp% * -1)
      .

END PROCEDURE.

PROCEDURE Kampanjehode_DatoTidAktiveres:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(DATETIME(Kampanjehode.StartDato, Kampanjehode.AktiveresTid * 1000),"99/99/9999 HH:MM:SS")
      .

END PROCEDURE.

PROCEDURE Kampanjehode_DatoTidSlutt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(DATETIME(Kampanjehode.SluttDato, Kampanjehode.GyldigTilTid * 1000),"99/99/9999 HH:MM:SS")
      .

END PROCEDURE.

PROCEDURE Kampanjehode_Operator:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    ASSIGN 
      ocValue = '-1'
      .

END PROCEDURE.

PROCEDURE AktiveresTid_time:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(KampanjeHode.AktiveresTid,"HH:MM")
      .

END PROCEDURE.

PROCEDURE GyldigTilTid_time:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Kampanjehode NO-LOCK
        WHERE ROWID(Kampanjehode) = irRowId
        NO-ERROR.
    IF AVAIL Kampanjehode THEN
      ASSIGN 
      ocValue = STRING(KampanjeHode.GyldigTilTid,"HH:MM")
      .

END PROCEDURE.







