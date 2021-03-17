DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError     AS CHAR NO-UNDO.

FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL VarebehLinje THEN 
  DO ON ERROR undo, LEAVE TRANSACTION:
    FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK 
        WHERE VarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
          AND VarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr:
      DELETE VarebehLinjeTrans.
    END.
    FOR EACH VarebehBestHode OF VarebehLinje EXCLUSIVE-LOCK:
      FOR EACH VarebehBestLinje EXCLUSIVE-LOCK OF VarebehBestHode:
        DELETE VarebehBestLinje.
      END.
      DELETE VarebehBestHode.
    END.
  END.
IF ERROR-STATUS:ERROR THEN
  ocError = "Feil oppsto ved sletting av Varebehandlingslinje(r).".
