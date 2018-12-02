PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:    
      cTid = STRING(SBudManed.eTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE DbSnitt:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE lDbSum AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lDbSnitt AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iDbAntall AS INTEGER NO-UNDO.
  
  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    FOR EACH SBudDag NO-LOCK WHERE
      SBudDag.SBudId = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd:
      IF SBudDag.DbProsent > 0 THEN
          ASSIGN  
          lDbSum    = lDbSum + SBudDag.DbProsent
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

PROCEDURE rTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:    
      cTid = STRING(SBudManed.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE linjeSum:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE lSum AS DECIMAL NO-UNDO.

  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    FOR EACH SBudDag NO-LOCK WHERE
      SBudDag.SBudId  = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd:
      IF SBudDag.SalgProsent <> ? THEN 
          lSum = lSum + SBudDag.SalgProsent.
    END.      
    ASSIGN 
      ocValue = STRING(lSum)
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

  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    FOR EACH SBudDag NO-LOCK WHERE
      SBudDag.SBudId  = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd:
      IF SBudDag.SalgProsent <> ? THEN 
          lSum = lSum + SBudDag.SalgProsent.
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


  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    ASSIGN 
      ocValue = SUBSTRING(STRING(SBudManed.AarMnd,'999999'),1,4)      
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
      
  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    ASSIGN 
      iMnd    = INT(SUBSTRING(STRING(SBudManed.AarMnd,'999999'),5,2))
      ocValue = ' ' + ENTRY (iMnd,cMndLst)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE iAntMndDbGT0:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
  DEFINE BUFFER tmpSBudManed FOR SBudManed.
  
  FIND SBudManed WHERE ROWID(SBudManed) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudManed THEN 
  DO:
    FOR EACH tmpSBudManed NO-LOCK WHERE
      tmpSBudManed.SBudId    = SBudManed.SBudId AND 
      tmpSBudManed.DbProsent > 0:
      iAnt = iAnt + 1.
    END.      
    ASSIGN 
      ocValue = STRING(iAnt)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.
END PROCEDURE.

