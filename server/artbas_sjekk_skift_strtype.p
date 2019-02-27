DEF INPUT  PARAM icArtBasRowid AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields      AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues      AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(icArtBasRowid) NO-LOCK NO-ERROR.
IF AVAIL ArtBas THEN DO:
  IF CAN-FIND(FIRST BestHode OF ArtBas WHERE BestHode.BestStat < 6 AND BestHode.TotAntPar > 0) THEN 
    ocReturn = "Det finnes bestillinger som ikke er full-levert for artikkel." + CHR(10) +
               "Endring av størrelsestype er ikke tillatt".

END.


