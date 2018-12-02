DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

FOR FIRST ArtBas FIELDS() NO-LOCK 
    WHERE ROWID(ArtBas) = irArtBas
      AND VareFakta NE "":
  ocReturn = "*".
END.
