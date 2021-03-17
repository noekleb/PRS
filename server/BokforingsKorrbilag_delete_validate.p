/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND BokforingsKorrBilag WHERE ROWID(BokforingsKorrBilag) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL BokforingsKorrBilag THEN DO:
  ocError = "Ukjent korreksjonsbilag, eller ikke tilgjengelig for sletting." + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.
ELSE DO:
  FIND Bokforingsbilag OF BokforingsKorrbilag NO-LOCK NO-ERROR.
  IF AVAILABLE Bokforingsbilag THEN 
  DO:
    IF (BokforingsBilag.GodkjentFlagg = TRUE OR 
        BokforingsBilag.SendtRegnskap = TRUE) THEN 
      ocError = 'Korreksjonsbilag kan ikke slettes på bokføringsbilag med denne status.'.
    ELSE
      ocError = ''.
  END.
  ELSE DO:
    ocError = 'Ukjent bokføringsbilag koblet til dette korreksjonsbilaget!'.
  END.
END.



 