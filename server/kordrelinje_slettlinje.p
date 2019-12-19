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
      
/*      /* Er det en RETURORdre som behandles, og vare er byttet på varelinjen også der, skal den også slettes. */*/
/*      IF KOrdreHode.SendingsNr = 'RETUR' AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN                          */
/*      RETURHANDTERING:                                                                                          */
/*      DO:                                                                                                       */
/*        FIND buf2KOrdreLinje EXCLUSIVE-LOCK WHERE                                                               */
/*          buf2KOrdreLinje.KOrdre_Id     = KOrdreHode.RefKOrdre_Id AND                                           */
/*          buf2KOrdreLinje.KOrdreLinjeNr =  bufKOrdreLinje.KOrdreLinjeNr NO-ERROR.                               */
/*        IF AVAILABLE buf2KOrdreLinje THEN                                                                       */
/*        DO:                                                                                                     */
/*          FIND buf3KOrdreLinje EXCLUSIVE-LOCK WHERE                                                             */
/*            buf3KOrdreLinje.KOrdre_Id     = KOrdreHode.RefKOrdre_Id AND                                         */
/*            buf3KOrdreLinje.KOrdreLinjeNr =  buf2KOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.                        */
/*          /* Aktiverer den gamle linjen */                                                                      */
/*          ASSIGN                                                                                                */
/*            buf3KOrdreLinje.Aktiv             = TRUE                                                            */
/*            buf3KOrdreLinje.KopiKOrdreLinjeNr = 0                                                               */
/*            .                                                                                                   */
/*          /* Sletter kopien */                                                                                  */
/*          DELETE buf2KOrdreLinje.                                                                               */
/*        END.                                                                                                    */
/*      END. /* RETURHANDTERING */                                                                                */
      
      /* Aktiverer den gamle linjen */  
      ASSIGN 
        KOrdreLinje.Aktiv             = TRUE
        KOrdreLinje.KopiKOrdreLinjeNr = 0
        .
      /* Sletter kopien */  
      DELETE bufKOrdreLinje.
    END.    
  END.

  ASSIGN 
    obOk     = TRUE 
    ocReturn = ""
    . 
    
END. /* TRANSACTION */
