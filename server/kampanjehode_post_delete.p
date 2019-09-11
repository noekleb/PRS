/*   kampanjehode_post_delete.p
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND Kampanjehode WHERE ROWID(Kampanjehode) = TO-ROWID(icRowid) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL Kampanjehode THEN DO:
  ocError = "Kampanje er ikke tilgjengelig for sletting" + CHR(10) + PROGRAM-NAME(1).
  RETURN.
END.
ELSE DO:
  FOR EACH KampanjeLinje OF KampanjeHode EXCLUSIVE-LOCK:
    DELETE KampanjeLinje.
  END.
  DELETE Kampanjehode.
END.
