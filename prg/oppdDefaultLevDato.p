/*------------------------------------------------------------------------------
  Purpose:     oppdEkstVPILev 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  {tmptt_DefaultLevDato.i &SHARED=SHARED}

  IF NOT CAN-FIND(FIRST TT_DefaultLevDato) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF DefaultLevDato OVERRIDE 
  DO:  
  END.
  ON WRITE OF DefaultLevDato OVERRIDE 
  DO:  
  END.
  ON DELETE OF DefaultLevDato OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_DefaultLevDato WHERE
    TT_DefaultLevDato.RecType = 3:
    FIND DefaultLevDato EXCLUSIVE-LOCK WHERE
      DefaultLevDato.ButikkNr = TT_DefaultLevDato.ButikkNr AND
      DefaultLevDato.LevNr    = TT_DefaultLevDato.LevNr NO-ERROR.
    IF AVAILABLE DefaultLevDato THEN
    DO:
        DELETE DefaultLevDato NO-ERROR.
        DELETE TT_DefaultLevDato.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  /* Når HK sender en leverandør, følger alltid alle levsort og alle */
  /* kontakter med.                                                  */
  NY-ENDRE:
  FOR EACH TT_DefaultLevDato WHERE
    TT_DefaultLevDato.RecType = 1:
    FIND DefaultLevDato EXCLUSIVE-LOCK WHERE
      DefaultLevDato.ButikkNr = TT_DefaultLevDato.ButikkNr AND
      DefaultLevDato.LevNr    = TT_DefaultLevDato.LevNr NO-ERROR.
    IF NOT AVAILABLE DefaultLevDato THEN
      CREATE DefaultLevDato.
    BUFFER-COPY TT_DefaultLevDato TO DefaultLevDato.
    RELEASE DefaultLevDato.
    DELETE TT_DefaultLevDato.
  END. /* NY-ENDRE */





