PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND FalckEksport WHERE ROWID(FalckEksport) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL FalckEksport THEN 
  DO:    
      cTid = STRING(FalckEksport.ETid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE EksportertTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND FalckEksport WHERE ROWID(FalckEksport) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL FalckEksport THEN 
  DO:    
      cTid = STRING(FalckEksport.EksportTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE registrertTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND FalckEksport WHERE ROWID(FalckEksport) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL FalckEksport THEN 
  DO:    
      cTid = STRING(FalckEksport.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

