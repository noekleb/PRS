DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND VareBokHode WHERE ROWID(VareBokHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL VareBokHode THEN DO:
  IF NOT CAN-FIND(FIRST VarebehHode
                  WHERE VarebehHode.kilde = VarebokHode.VarebokNr) THEN DO:
    DO ON ERROR undo, LEAVE TRANSACTION:
        FOR EACH VareBokLinje EXCLUSIVE-LOCK OF VareBokHode:
            DELETE VareBokLinje.
        END.
    END.
    IF ERROR-STATUS:ERROR THEN
        ocError = "Feil oppsto ved sletting av vareboklinje(r).".
  END.
  ELSE 
    ocError = "Kan ikke slette varebok hvis det er generert varehåndteringsbok".
END.
