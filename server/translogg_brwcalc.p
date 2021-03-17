/* translogg_brwcalc.p */

DEF VAR iCl            AS INT NO-UNDO.
DEFINE VARIABLE cTotalt AS CHARACTER NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.



/* **********************  Internal Procedures  *********************** */

PROCEDURE translogg_BruttoPris:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      IF AVAILABLE TransLogg THEN 
        ASSIGN 
          ocValue = STRING(TransLogg.Pris - TransLogg.RabKr)
          .
    END.

END PROCEDURE.

PROCEDURE translogg_PostertTid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      IF AVAILABLE TransLogg THEN 
        ASSIGN 
          ocValue = STRING(TransLogg.PostertTid,"HH:MM:SS")
          .
    END.

END PROCEDURE.

PROCEDURE translogg_Tid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      IF AVAILABLE TransLogg THEN 
        ASSIGN 
          ocValue = STRING(TransLogg.Tid,"HH:MM:SS")
          .
    END.

END PROCEDURE.

PROCEDURE translogg_TransType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      FIND TransType NO-LOCK OF 
        TransLogg NO-ERROR.
      IF AVAILABLE Transtype THEN 
        ASSIGN 
          ocValue = TransType.Beskrivelse
          .
    END.

END PROCEDURE.

PROCEDURE translogg_LevKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ASSIGN 
          ocValue = ArtBas.LevKod
          .
    END.

END PROCEDURE.

PROCEDURE translogg_LevFargKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irRowid  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Translogg NO-LOCK
        WHERE ROWID(Translogg) = irRowId
        NO-ERROR.
    IF AVAIL Translogg THEN
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN 
        ASSIGN 
          ocValue = ArtBas.LevFargKod
          .
    END.

END PROCEDURE.






