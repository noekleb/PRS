/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND Ovbunt WHERE ROWID(Ovbunt) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL Ovbunt THEN DO:
  ocError = "Reservasjonen er ikke tilgjengelig for sletting" + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.
ELSE DO:
  IF CAN-DO('10,50,60',STRING(OvBunt.BuntStatus)) AND 
     OvBunt.opphav     = 10 THEN 
     ocError = ''.
  ELSE  
     ocError = 'Reservasjonen kan ikke slettes.'.
END.



