PROCEDURE sjekkType:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
  
  ASSIGN
    cType = ''.
  
  FIND MedRabSjekk WHERE ROWID(MedRabSjekk) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedRabSjekk THEN 
  DO:    
      FIND RabSjekkType OF MedRabSjekk NO-LOCK NO-ERROR.
      IF AVAILABLE RabSjekkType THEN
          cType = RabSjekkType.RabSjekkTypeBeskrivelse.
  END.
  ASSIGN 
    ocValue   = cType
  . 
END PROCEDURE.

PROCEDURE medlemNavn:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cNavn AS CHARACTER NO-UNDO.
  
  ASSIGN
    cNavn = ''.
  
  FIND MedRabSjekk WHERE ROWID(MedRabSjekk) = irRowid NO-LOCK NO-ERROR.
  IF AVAILABLE MedRabSjekk THEN 
    FIND Medlem OF MedRabSjekk NO-LOCK NO-ERROR.
  IF AVAIL Medlem THEN 
  DO:    
      cNavn = Medlem.Fornavn + (IF Medlem.ForNavn = '' THEN '' ELSE ',') + Medlem.Etternavn.
  END.
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
  
  FIND MedRabSjekk WHERE ROWID(MedRabSjekk) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedRabSjekk THEN 
  DO:    
      cTid = STRING(MedRabSjekk.ETid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.

PROCEDURE TidUtstedt:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
 
  DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
  
  ASSIGN
    cTid = '  :  '.
  
  FIND MedRabSjekk WHERE ROWID(MedRabSjekk) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedRabSjekk THEN 
  DO:    
      cTid = STRING(MedRabSjekk.TidUtstedt,'HH:MM').
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
  
  FIND MedRabSjekk WHERE ROWID(MedRabSjekk) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL MedRabSjekk THEN 
  DO:    
      cTid = STRING(MedRabSjekk.RegistrertTid,'HH:MM').
  END.
  ASSIGN 
    ocValue   = cTid
  . 
  
END PROCEDURE.
