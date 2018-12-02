DEF INPUT  PARAM irKOrdreLinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

FOR FIRST KOrdreLinje NO-LOCK 
    WHERE ROWID(KOrdreLinje) = irKOrdreLinje
    ,FIRST ArtBas NO-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.Varenr)
    ,FIRST KOrdreHode OF KOrdreLinje NO-LOCK
    :
  RUN art_vgrabatt%.p(ROWID(ArtBas),STRING(KOrdreHode.KundeNr),icSessionId,OUTPUT ocValue).
END.
