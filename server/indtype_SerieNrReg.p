DEF INPUT PARAM irIndType AS ROWID NO-UNDO.
DEF INPUT PARAMETER  cSessionId        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue   AS CHAR NO-UNDO.

FIND IndType WHERE ROWID(IndType) = irIndType NO-LOCK NO-ERROR.
IF AVAIL IndType THEN
  ocValue = ENTRY(indType.SerieNrReg,"1-Ingen spørsmål,2-Tvang på Serienr,3-Spørsmål om serienr").
