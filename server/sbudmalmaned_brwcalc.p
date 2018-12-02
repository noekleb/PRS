PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:    
      cTid = STRING(SBudMalManed.eTid,'HH:MM').
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
  
  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:    
      cTid = STRING(SBudMalManed.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE iAntMndDbGT0:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
  DEFINE BUFFER tmpSBudMalManed FOR SBudMalManed.
  
  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    FOR EACH tmpSBudMalManed NO-LOCK WHERE
      tmpSBudMalManed.MalId     = SBudMalManed.MalId AND 
      tmpSBudMalManed.DbProsent > 0:
      iAnt = iAnt + 1.
    END.      
    ASSIGN 
      ocValue = STRING(iAnt)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.
END PROCEDURE.

PROCEDURE linjeSum:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE lSum AS DECIMAL NO-UNDO.

  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    FOR EACH SBudMalDag NO-LOCK WHERE
      SBudMalDag.MalId  = SBudMalManed.MalId AND 
      SBudMalDag.AarMnd = SBudMalManed.AarMnd:
      IF SBudMalDag.Prosent <> ? THEN 
          lSum = lSum + SBudMalDag.Prosent.
    END.      
    ASSIGN 
      ocValue = STRING(lSum)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.
END.

PROCEDURE DbSnitt:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE lDbSum AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lDbSnitt AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iDbAntall AS INTEGER NO-UNDO.
  
  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    FOR EACH SBudMalDag NO-LOCK WHERE
      SBudMalDag.MalId  = SBudMalManed.MalId AND 
      SBudMalDag.AarMnd = SBudMalManed.AarMnd:
      IF SBudMalDag.DbProsent > 0 THEN
          ASSIGN  
          lDbSum    = lDbSum + SBudMalDag.DbProsent
          iDbAntall = iDbAntall + 1.
    END.      
    ASSIGN 
      lDbSnitt = ROUND(lDbSum / iDbAntall,2)
      lDbSnitt = IF lDbSnitt = ? THEN 0 ELSE lDbSnitt
      ocValue  = STRING(lDbSnitt)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.
END.

PROCEDURE chkProc:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE lSum AS DECIMAL NO-UNDO.

  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    FOR EACH SBudMalDag NO-LOCK WHERE
      SBudMalDag.MalId  = SBudMalManed.MalId AND 
      SBudMalDag.AarMnd = SBudMalManed.AarMnd:
      IF SBudMalDag.Prosent <> ? THEN 
          lSum = lSum + SBudMalDag.Prosent.
    END.      
    ASSIGN 
      ocValue = STRING(lSum = 100)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.

END PROCEDURE.

PROCEDURE tmpAar:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.


  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    ASSIGN 
      ocValue = SUBSTRING(STRING(SBudMalManed.AarMnd,'999999'),1,4)      
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.

END PROCEDURE.

PROCEDURE tmpMnd:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cMndLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMnd    AS INTEGER   NO-UNDO.
    
    {syspara.i 23 1 1 cMndLst}
    IF cMndLst = '' THEN cMndLst = 'JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES'.

  FIND SBudMalManed WHERE ROWID(SBudMalManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalManed THEN 
  DO:
    ASSIGN 
      iMnd    = INT(SUBSTRING(STRING(SBudMalManed.AarMnd,'999999'),5,2))
      ocValue = ' ' + ENTRY (iMnd,cMndLst)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      NO-ERROR.
  END.

END PROCEDURE.

