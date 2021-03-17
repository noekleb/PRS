PROCEDURE endretTid:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:    
      cTid = STRING(SBudMalDag.eTid,'HH:MM').
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
  
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:    
      cTid = STRING(SBudMalDag.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE tmpiMDag:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.


  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:
    ASSIGN 
      ocValue = SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),7,2)      
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      .
  END.

END PROCEDURE.

PROCEDURE tmpdMDag:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE dDato   AS DATE NO-UNDO.
  
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:
    ASSIGN 
      dDato   = DATE (INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),5,2)), 
                 INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),7,2)), 
                 INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),1,4)))
      ocValue = STRING(dDato)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE tmpcUDag:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cDagLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iWDay   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dDato   AS DATE NO-UNDO.
  
    {syspara.i 23 1 2 cDagLst}
    IF cDagLst = '' THEN cDagLst = 'SØN,MAN,TIR,ONS,TOR,FRE,LØR'.
      
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:
    ASSIGN 
      dDato   = DATE (INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),5,2)), 
                 INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),7,2)), 
                 INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),1,4)))
      iWDay   = WEEKDAY(dDato)
      ocValue = ' ' + ENTRY (iWDay,cDagLst)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      NO-ERROR.
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
      
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:
    ASSIGN 
      iMnd    = INT(SUBSTRING(STRING(SBudMalDag.AarMndDag,'99999999'),5,2))
      ocValue = ' ' + ENTRY (iMnd,cMndLst)
      /*ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue*/
      NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE iAntDagDbGT0:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
  DEFINE BUFFER tmpSBudMalDag FOR SBudMalDag.
  
  FIND SBudMalDag WHERE ROWID(SBudMalDag) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL SBudMalDag THEN 
  DO:
    FOR EACH tmpSBudMalDag NO-LOCK WHERE
      tmpSBudMalDag.MalId     = SBudMalDag.MalId AND 
      tmpSBudMalDag.AarMnd    = SBudMalDag.AarMnd AND 
      tmpSBudMalDag.DbProsent > 0:
      iAnt = iAnt + 1.
    END.      
    ASSIGN 
      ocValue = STRING(iAnt)
      .
  END.
END PROCEDURE.




