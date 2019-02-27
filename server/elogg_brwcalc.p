

PROCEDURE eTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND Elogg WHERE ROWID(ELogg) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ELogg THEN 
  DO:    
      cTid = STRING(ELogg.eTid,'HH:MM:SS').
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
  
  FIND ELogg WHERE ROWID(ELogg) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ELogg THEN 
  DO:    
      cTid = STRING(ELogg.RegistrertTid,'HH:MM:SS').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.
