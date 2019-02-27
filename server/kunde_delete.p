DEF INPUT  PARAM icBufferName        AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

FIND FIRST Kunde WHERE ROWID(Kunde) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Kunde THEN DO:
  IF CAN-FIND(FIRST FakturaHode OF Kunde) OR 
     CAN-FIND(FIRST Kundereskontr OF Kunde) 
     THEN ocReturn = "Kunden har transaksjoner og kan ikke slettes".
  ELSE DO:
    FOR EACH KundeKort OF Kunde EXCLUSIVE-LOCK:
      DELETE KundeKort.
    END.
  END.
END.
ELSE ocReturn = "Kunde ikke tilgjengelig for sletting".
