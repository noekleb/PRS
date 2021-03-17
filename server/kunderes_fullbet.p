DEF INPUT  PARAM icKundereskontrRowid AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields             AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues             AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId          AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn             AS CHAR NO-UNDO.

DEF VAR bOK        AS LOG    NO-UNDO.

FIND Kundereskontr 
     WHERE ROWID(Kundereskontr) = TO-ROWID(icKundereskontrRowid)
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Kundereskontr THEN 
  IF icFields = "InnbetDato" THEN DO:
    RUN kunderes_innbet.p (STRING(Kundereskontr.KundeNr) + "|"
                          + STRING(Kundereskontr.Reskontro_id) + "|"
                          + "0|"
                          + STRING(icValues) + "|"
                          + STRING(Kundereskontr.Saldo) + "|"
                          + STRING(Kundereskontr.KID) + "|"
                          + "|0|0",
                          ?,
                          icSessionId,
                          OUTPUT ocReturn,
                          OUTPUT bOK).
END.
ELSE 
  ocReturn = "Kundereskontr ikke tilgj. for oppdatering".

