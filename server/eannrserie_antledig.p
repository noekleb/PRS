  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR piAnt AS INT NO-UNDO.
  
  FIND EANNrSerie WHERE ROWID(EANNrSerie) = irRowid NO-LOCK NO-ERROR.

  FOR EACH EANNrListe OF EANNrSErie NO-LOCK WHERE
    EANNrListe.EANSerieId = EANNrSErie.EANSerieId AND
    EANNrListe.ArtikkelNr = 0:
    piAnt = piAnt + 1.
  END.
  
  ocValue = STRING(piAnt).


