DEF INPUT PARAM irGavekort AS ROWID NO-UNDO.
DEF OUTPUT PARAM ocValue   AS CHAR NO-UNDO.

FIND Gavekort WHERE ROWID(Gavekort) = irGavekort NO-LOCK NO-ERROR.
IF AVAIL Gavekort THEN
  ocValue = STRING(BruktTid,"HH:MM").
