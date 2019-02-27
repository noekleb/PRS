
PROCEDURE medlemNavn:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cNavn AS CHARACTER NO-UNDO.
  
  ASSIGN
    cNavn = ''.
  
  FIND MedKjop WHERE ROWID(MedKjop) = irRowid NO-LOCK NO-ERROR.
  IF AVAILABLE MedKjop THEN 
    FIND Medlem OF MedKjop NO-LOCK NO-ERROR.
  IF AVAIL Medlem THEN 
  DO:    
      cNavn = Medlem.Fornavn + (IF Medlem.ForNavn = '' THEN '' ELSE ',') + Medlem.Etternavn.
  END.
  ELSE
      cNavn = '** Ukjent'.
  ASSIGN 
    ocValue   = cNavn
  . 
  
END PROCEDURE.

PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND MedKjop WHERE ROWID(MedKjop) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedKjop THEN 
  DO:    
      cTid = STRING(MedKjop.ETid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE medkjopsTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND MedKjop WHERE ROWID(MedKjop) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedKjop THEN 
  DO:    
      cTid = STRING(MedKjop.KjopsTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE rTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND MedKjop WHERE ROWID(MedKjop) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedKjop THEN 
  DO:    
      cTid = STRING(MedKjop.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.
