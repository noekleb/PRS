/*------------------------------------------------------------------------------
  Purpose:     oppdEkstVPILev 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iHKVareId AS INT  NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.
  
{tmptt_EkstVPILev.i &SHARED=SHARED}

IF NOT CAN-FIND(FIRST TT_EkstVPILev) THEN
  RETURN.

/* Behandler sletteposter */
SLETTEPOSTER:
FOR EACH TT_EkstVPILev WHERE
  TT_EkstVPILev.RecType = 3:
  FIND EkstVPILev EXCLUSIVE-LOCK WHERE
    EkstVPILev.EkstVPILevNr = TT_EkstVPILev.EkstVPILevNr NO-ERROR.
  IF AVAILABLE EkstVPILev THEN
  DO:
      DELETE EkstVPILev NO-ERROR.
      DELETE TT_EkstVPILev.
  END.
END. /* SLETTEPOSTER */

/* Behandler Ny/endre poster */
NY-ENDRE:
FOR EACH TT_EkstVPILev WHERE
  TT_EkstVPILev.RecType = 1:
  /* Ordinært oppslag */
  FIND EkstVPILev EXCLUSIVE-LOCK WHERE
    EkstVPILev.EkstVPILevNr = TT_EkstVPILev.EkstVPILevNr NO-ERROR.
  IF NOT AVAILABLE EkstVPILev THEN
    CREATE EkstVPILev.
  BUFFER-COPY TT_EkstVPILev 
      EXCEPT AktivLev
      TO EkstVPILev NO-ERROR.
  /* Det har forekommet at at det har ligget lokalt opprettede EkstVPILev.... */
  IF ERROR-STATUS:ERROR AND AVAILABLE EkstVPILev 
      THEN DELETE EkstVPILev.
  ELSE
      RELEASE EkstVPILev.
  DELETE TT_EkstVPILev.
END. /* NY-ENDRE */




