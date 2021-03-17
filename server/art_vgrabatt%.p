DEF INPUT  PARAM irArtBas      AS ROWID NO-UNDO.
DEF INPUT  PARAM icKundeNr     AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

FOR FIRST ArtBas NO-LOCK 
    WHERE ROWID(ArtBas) = irArtBas
      AND ArtBas.KundeRabatt
    ,FIRST Kunde WHERE Kunde.KundeNr = DEC(icKundeNr) NO-LOCK
    ,FIRST VgKundeGrpRabatt NO-LOCK
           WHERE VgKundeGrpRabatt.GruppeId = Kunde.GruppeId
             AND VgKundeGrpRabatt.Vg = ArtBas.Vg
    :

  ocValue = STRING(VgKundeGrpRabatt.Rabatt%).
END.
