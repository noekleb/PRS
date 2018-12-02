DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND Ordre WHERE ROWID(Ordre) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Ordre AND Ordre.OrdreStatus < 2 THEN DO:
  FOR EACH VarebehBestHode EXCLUSIVE-LOCK
      WHERE VareBehBestHode.OrdreNr = Ordre.OrdreNr:
    FOR EACH VarebehBestLinje OF VareBehBestHode EXCLUSIVE-LOCK:
      DELETE VarebehBestLinje.
    END.
    DELETE VareBehBestHode.
  END.
  FOR EACH BestHode EXCLUSIVE-LOCK
      WHERE BestHode.OrdreNr = Ordre.OrdreNr:

    FOR EACH BestLinje OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestLinje.
    END.
    FOR EACH BestPris OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestPris.
    END.
    FOR EACH BestSort OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestSort.
    END.
    FOR EACH BestStr OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestStr.
    END.
    FOR EACH BestKasse OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestKasse.
    END.

    DELETE BestHode.     
  END.
  DELETE Ordre.
END.
ELSE IF AVAIL Ordre AND Ordre.OrdreStatus > 1 THEN
  ocError = "Ordre er sendt og kan derfor ikke slettes".
ELSE IF NOT AVAIL Ordre THEN
  ocError = "Ordre er ikke tilgjengelig for sletting" + CHR(10) + PROGRAM-NAME(1).
