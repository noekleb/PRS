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
DEFINE BUFFER buf2KOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER buf3KOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKOrdreHode FOR KOrdrEHode.

/* RowId på kordrelinje som skal kopieres og deaktiveres. */
cRowId = ENTRY(1,icParam,'|').

FIND KOrdreHode NO-LOCK WHERE 
  KOrdreHode.KOrdre_Id = DEC(ENTRY(1,icParam)) NO-ERROR.

IF (NUM-ENTRIES(KOrdreHode.EkstOrdreNr,' ') = 1 OR  
    KOrdreHode.EkstOrdreNr MATCHES '*RETUR*') THEN 
DO TRANSACTION:
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE
    bufKOrdreLinje.KOrdre_Id = DEC(ENTRY(1,icParam)) AND 
    bufKOrdreLinje.KOrdreLinjeNr = INT(ENTRY(2,icParam)) NO-ERROR.
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
    END.    
    /* Sletter kopien */  
    DELETE bufKOrdreLinje.
  END.

  ASSIGN 
    obOk     = TRUE 
    ocReturn = ""
    . 
    
END. /* TRANSACTION */

ELSE IF KOrdreHode.EkstOrdreNr MATCHES '*BYTT*' THEN 
DO TRANSACTION:
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE
    bufKOrdreLinje.KOrdre_Id = DEC(ENTRY(1,icParam)) AND 
    bufKOrdreLinje.KOrdreLinjeNr = INT(ENTRY(2,icParam)) NO-ERROR.
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
      KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.ByttetKOrdreLinjeNr NO-ERROR.
    IF AVAILABLE KOrdreLinje THEN 
    DO: 
      /* Aktiverer den gamle linjen */  
      ASSIGN 
        KOrdreLinje.Aktiv               = TRUE
        KOrdreLinje.ByttetKOrdreLinjeNr = 0
        .
    END.
    
    /* Sletter kopien */  
    DELETE bufKOrdreLinje.        
  END.

  ASSIGN 
    obOk     = TRUE 
    ocReturn = ""
    . 
    
END. /* TRANSACTION */
