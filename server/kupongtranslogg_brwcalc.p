
PROCEDURE tmpAar:
    DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
    DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

    ocValue = ''.

END PROCEDURE.

PROCEDURE rcSalgsTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND KupongTransLogg WHERE ROWID(KupongTransLogg) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KupongTransLogg THEN 
  DO:    
      cTid = STRING(KupongTransLogg.SalgsTid,'HH:MM:SS').
  END.
  ASSIGN 
    ocValue   = cTid
  . 

END PROCEDURE.
