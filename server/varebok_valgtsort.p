DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR fArtikkelnr AS DEC NO-UNDO.
DEF VAR fVareboknr  AS DEC NO-UNDO.
DEF VAR fMessenr    AS DEC NO-UNDO.

FOR EACH VarebokLinje FIELDS() NO-LOCK
    WHERE ROWID(VarebokLinje) = irVarebokLinje
    ,FIRST ArtSort WHERE ArtSort.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK:
  ocReturn = "*".
END.
