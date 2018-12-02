PROCEDURE _Total:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR fTotal                AS DEC   NO-UNDO.

  FIND Reklamasjonslogg WHERE ROWID(Reklamasjonslogg) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL Reklamasjonslogg THEN 
  DO:    
    FOR EACH Reklamasjonslinje OF Reklamasjonslogg NO-LOCK:
      fTotal = fTotal + (Reklamasjonslinje.Pris - Reklamasjonslinje.RabKr * Reklamasjonslinje.antall). 
    END.
  END.
  ocValue = STRING(fTotal).
END PROCEDURE.
