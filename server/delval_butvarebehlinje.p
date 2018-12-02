DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DO ON ERROR undo, LEAVE TRANSACTION:
  FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FOR EACH VarebehBestHode OF VarebehLinje EXCLUSIVE-LOCK:
      RUN delval_varebehbesthode.p ("VarebehBestHode",
                                     STRING(ROWID(VarebehBestHode)),
                                     icSessionId,
                                     OUTPUT ocReturn).
      IF ocReturn NE "" THEN UNDO, LEAVE.

      FOR EACH VarebehBestLinje EXCLUSIVE-LOCK OF VarebehBestHode:
        DELETE VarebehBestLinje.
      END.
      IF AVAIL VarebehBestHode THEN DELETE VarebehBestHode.
    END.
  END.
END.
IF ocReturn NE "" THEN
  ocReturn = "Feil oppsto ved sletting av Varebehandlingslinje(r):" + CHR(10)
           + ocReturn.
