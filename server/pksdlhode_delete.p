DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

FIND PkSdlHode WHERE ROWID(PkSdlHode) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL PkSdlHode AND PkSdlHode.PkSdlStatus LE 10 THEN 
DO ON ERROR undo, LEAVE TRANSACTION:
  FOR EACH PkSdlLinje EXCLUSIVE-LOCK OF PkSdlHode:
    DELETE PkSdlLinje.
  END.
  FOR EACH PkSdlPris EXCLUSIVE-LOCK OF PkSdlHode:
    DELETE PkSdlPris.
  END.
  FOR EACH PkSdlMottak EXCLUSIVE-LOCK OF PkSdlHode:
    DELETE PkSdlMottak.
  END.
END.
IF ERROR-STATUS:ERROR THEN
  ocReturn = "Feil oppsto ved sletting av Pakkseddel".
