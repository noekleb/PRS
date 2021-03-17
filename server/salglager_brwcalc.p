/* salglager_brwcalc.p */
/* NB: 12/5-20 TN Denne rutinen kjøres ikke når localdata er aktivert. */
/* **********************  Internal Procedures  *********************** */

PROCEDURE Lager_Vg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM rRowId AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam  AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Lager NO-LOCK
        WHERE ROWID(Lager) = rRowId
        NO-ERROR.
        
    IF AVAIL Lager THEN
    ARTBLOKK:
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN 
        LEAVE ARTBLOKK.
      IF icParam = '' THEN 
        ocValue = STRING(ArtBas.Vg).
      ELSE IF CAN-DO(icParam,STRING(ArtBas.Vg)) THEN 
        ocValue = STRING(ArtBas.Vg).
      ELSE 
        ocValue = 'SKIPROW'.
    END.

END PROCEDURE.

PROCEDURE Lager_VmId:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM rRowId AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam  AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND Lager NO-LOCK
        WHERE ROWID(Lager) = rRowId
        NO-ERROR.
        
    IF AVAIL Lager THEN
    ARTBLOKK:
    DO:
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN 
        LEAVE ARTBLOKK.
      IF icParam = '' THEN 
        ocValue = STRING(ArtBas.VmId).
      ELSE IF CAN-DO(icParam,STRING(ArtBas.VmId)) THEN 
        ocValue = STRING(ArtBas.VmId).
      ELSE 
        ocValue = 'SKIPROW'.
    END.

END PROCEDURE.










