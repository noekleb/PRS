DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF BUFFER bBestHode FOR BestHode.

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL VarebehBestHode THEN 
DO ON ERROR UNDO, LEAVE:
  FIND BestHode
       WHERE BestHode.BestNr = VarebehBestHode.BestNr 
       EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAIL BestHode THEN DO:
    IF LOCKED BestHode THEN DO:
      ocReturn = "Bestillingshode er ikke tilgjengelig for sletting".
      RETURN.
    END.
  END.
  ELSE IF BestHode.BestStat > 3 THEN DO:
    /* Første element benyttes her til å styre program-logikk - må ikke endres */
    ocReturn = "sendt;Bestilling er sendt og kan ikke slettes".
    RETURN.
  END.
  ELSE DO:
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
    FOR EACH Fributik OF BestHode
        EXCLUSIVE-LOCK:
      DELETE Fributik.
    END.
    FOR EACH BestKasse OF BestHode
        EXCLUSIVE-LOCK:
      DELETE BestKasse.
    END.

    FIND FIRST bBestHode NO-LOCK
         WHERE bBestHode.OrdreNr  = BestHode.OrdreNr
           AND bBestHode.BestNr   NE BestHode.BestNr
         NO-ERROR.
    IF NOT AVAIL bBestHode THEN DO:
      FIND Ordre WHERE Ordre.OrdreNr = BestHode.OrdreNr EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL Ordre THEN DELETE Ordre.
      ELSE IF LOCKED Ordre THEN DO:
        ocReturn = "Ordre " + STRING(BestHode.OrdreNr) + " ikke tilgjengelig for sletting" + CHR(10) +
                   PROGRAM-NAME(1).
        UNDO, LEAVE.
      END.
    END.

    DELETE BestHode.
  END.

  FOR EACH VarebehBestLinje EXCLUSIVE-LOCK OF VarebehBestHode:
    DELETE VarebehBestLinje.
  END.
  DELETE VarebehBestHode.
END.
