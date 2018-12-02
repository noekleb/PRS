/* Sikrer at bare siste status på bestilling vises
------------------------------------------------------------------------------------------------------*/  
DEF INPUT PARAM  irBuffer    AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam     AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF BUFFER bBestStr FOR BestStr.

FIND BestStr WHERE ROWID(BestStr) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL BestStr THEN DO:
  FIND FIRST bBestStr NO-LOCK
       WHERE bBestStr.BestNr    = BestStr.BestNr
         AND bBestStr.Butik     = BestStr.Butik
         AND bBestStr.Storl     = BestStr.Storl
         AND bBestStr.BestStat  > BestStr.BestStat
       NO-ERROR.
  IF AVAIL bBestStr THEN
    ocValue = "skiprow".
END.

