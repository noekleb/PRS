DEF INPUT  PARAM icBufferName        AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEFINE BUFFER bInnloser FOR Innloser.

FIND FIRST bInnloser WHERE ROWID(bInnloser) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL bInnloser THEN DO:
  IF TRIM(bInnloser.KortNavn) <> '' AND CAN-FIND(FIRST TransBeskr WHERE TransBeskr.KortNavn = bInnloser.KortNavn)
     THEN ocReturn = "Det ligger transaksjonsbeskrivelser knyttet til denne innløser. Kan ikke slettes".
  ELSE DO:
    /*
    FOR EACH bInnloserKort OF bInnloser EXCLUSIVE-LOCK:
      DELETE bInnloserKort.
    END.
    */
  END.
END.
ELSE ocReturn = "Innloser ikke tilgjengelig for sletting".
