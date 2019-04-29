/* kordrelinje_slettvare.p
    
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKOrdreLinjeNr AS INTEGER NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

/* RowId på kordrelinje som skal kopieres og deaktiveres. */
cRowId = ENTRY(1,icParam,'|').

DO TRANSACTION:
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE
    bufKOrdreLinje.KOrdre_Id = DEC(ENTRY(1,icParam)) AND 
    bufKOrdreLinje.KOrdrELinjeNr = INT(ENTRY(2,icParam)) NO-ERROR.
  IF NOT AVAILABLE bufKOrdreLinje THEN 
  DO:
    ASSIGN 
      obOk     = FALSE 
      ocReturn = "** Ukjent KOrdreLinje mottatt i 'kordrelinje_slettlinje.p' ( " + cRowId + " ): " + ERROR-STATUS:GET-MESSAGE(1)
      . 
  END. 
  ELSE DO:
    FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
      KOrdreLinje.KOrdre_Id     = bufKOrdreLinje.KOrdre_Id AND 
      KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
    IF AVAILABLE KOrdreLinje THEN 
    DO:
      /* Aktiverer den gamle linjen */
      ASSIGN 
        KOrdreLinje.Aktiv             = TRUE
        KOrdreLinje.KopiKOrdreLinjeNr = 0
        .
      /* Sletter kopien */  
      DELETE bufKOrdreLinje.
    END.    
  END.
    
END. /* TRANSACTION */
