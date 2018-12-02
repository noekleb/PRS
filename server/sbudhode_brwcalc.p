
PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SBudHode WHERE ROWID(SBudHode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudHode THEN 
  DO:    
      /*cTid = STRING(SBudManed.eTid,'HH:MM').*/
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
  
  FIND SBudHode WHERE ROWID(SBudHode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudHode THEN 
  DO:    
      cTid = STRING(SBudHode.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE tmpDbProc:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  DEFINE VARIABLE lProc AS DECIMAL NO-UNDO.

  FIND SBudHode WHERE ROWID(SBudHode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudHode THEN 
  DO:
    ASSIGN 
      lProc = ROUND(
                    (SBudHode.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudHode.SalgBudsjett,0)) * 100
                   ,2)
      lProc = IF lProc = ? THEN 0 ELSE lProc
      ocValue = STRING(lProc).
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.
END.

PROCEDURE iAntBud:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

   ocValue = '1'.
END.

