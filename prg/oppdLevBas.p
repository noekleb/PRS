/*------------------------------------------------------------------------------
  Purpose:     oppdEkstVPILev 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {tmptt_LevBas.i &SHARED=SHARED}

  IF NOT CAN-FIND(FIRST TT_LevBas) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF LevBas OVERRIDE 
  DO:  
  END.
  ON WRITE OF LevBas OVERRIDE 
  DO:  
  END.
  ON DELETE OF LevBas OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_LevBas WHERE
    TT_LevBas.RecType = 3:
    FIND LevBas EXCLUSIVE-LOCK WHERE
      LevBas.LevNr = TT_LevBas.LevNr NO-ERROR.
    IF AVAILABLE LevBas THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF LevBas) THEN
      DO:
        FOR EACH LevSort OF LevBas:
            FOR EACH LevSAnt OF LevSort:
                DELETE LevSAnt.
            END.
            DELETE LevSort.
        END.
        FOR EACH LevKontakt OF LevBas:
            DELETE LevKontakt.
        END.
        DELETE LevBas NO-ERROR.
        DELETE TT_LevBas.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  /* Når HK sender en leverandør, følger alltid alle levsort og alle */
  /* kontakter med.                                                  */
  NY-ENDRE:
  FOR EACH TT_LevBas WHERE
    TT_LevBas.RecType = 1:
    FIND LevBas EXCLUSIVE-LOCK WHERE
      LevBas.LevNr = TT_LevBas.LevNr NO-ERROR.
    IF NOT AVAILABLE LevBas THEN
      CREATE LevBas.
    ELSE DO:
        FOR EACH LevSort OF LevBas:
            FOR EACH LevSAnt OF LevSort:
                DELETE LevSAnt.
            END.
            DELETE LevSort.
        END.
        FOR EACH LevKontakt OF LevBas:
            DELETE LevKontakt.
        END.
    END.
    BUFFER-COPY TT_LevBas TO LevBas.
    RELEASE LevBas.
    DELETE TT_LevBas.
  END. /* NY-ENDRE */





