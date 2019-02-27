PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SIEMoms WHERE ROWID(SIEMoms) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SIEMoms THEN 
  DO:    
      cTid = STRING(SIEMoms.ETid,'HH:MM').
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
  
  FIND SIEMoms WHERE ROWID(SIEMoms) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SIEMoms THEN 
  DO:    
      cTid = STRING(SIEMoms.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE MomsProc:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cMomsProc AS CHARACTER NO-UNDO.
  
  ASSIGN
    cMomsProc = ''.
  
  FIND SIEMoms WHERE ROWID(SIEMoms) = irRowid NO-LOCK NO-ERROR.
  FIND Moms NO-LOCK WHERE Moms.MomsKod = SIEMoms.MomsKod NO-ERROR.
  
  IF AVAIL Moms THEN 
  DO:    
      cMomsProc = STRING(Moms.MomsProc).
  END.
  ASSIGN 
    ocValue   = cMomsProc
  . 
  
END PROCEDURE.
