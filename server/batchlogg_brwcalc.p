/* batchlogg_brwcalc.p */

DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE cTotalt AS CHARACTER NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.



/* **********************  Internal Procedures  *********************** */


PROCEDURE BatchLogg_DatoTidOppdatert:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND BatchLogg NO-LOCK
        WHERE ROWID(BatchLogg) = irRowId
        NO-ERROR.
    IF AVAIL BatchLogg THEN
      ASSIGN 
      ocValue = STRING(DATETIME(BatchLogg.StatusOppdatert, BatchLogg.TidOppdatert * 1000),"99/99/9999 HH:MM:SS")
      .

END PROCEDURE.

PROCEDURE BatchLogg_DatoTidOpprettet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND BatchLogg NO-LOCK
        WHERE ROWID(BatchLogg) = irRowId
        NO-ERROR.
    IF AVAIL BatchLogg THEN
      ASSIGN 
      ocValue = STRING(DATETIME(BatchLogg.RegistrertDato, BatchLogg.RegistrertTid * 1000),"99/99/9999 HH:MM:SS")
      .
END PROCEDURE.

PROCEDURE BatchLogg_DatoTidEndret:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND BatchLogg NO-LOCK
        WHERE ROWID(BatchLogg) = irRowId
        NO-ERROR.
    IF AVAIL BatchLogg THEN
      ASSIGN 
      ocValue = STRING(DATETIME(BatchLogg.EDato, BatchLogg.ETid * 1000),"99/99/9999 HH:MM:SS")
      .

END PROCEDURE.








