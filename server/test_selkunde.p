DEF INPUT PARAM icRowid         AS CHAR NO-UNDO.
DEF INPUT PARAM icDummyFields   AS CHAR NO-UNDO.
DEF INPUT PARAM icDummyValues   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFieldsAndVal AS CHAR NO-UNDO.

FIND kunde WHERE ROWID(kunde) = TO-ROWID(icRowid) NO-ERROR.
IF AVAIL kunde THEN
  ocFieldsAndVal = "valuelist|Navn|" + kunde.navn + " - selected".


