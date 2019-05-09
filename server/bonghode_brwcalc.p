/* Bibliotek for kalkulerte felter, kundeordrelinje
  Opprettet: 
------------------------------------------------------------------------------*/  

PROCEDURE bongHode_DatoTid:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN 
    ocValue = STRING(DATETIME(BongHode.Dato, BongHode.Tid * 1000), "99/99-9999 HH:MM:SS").
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_eAv:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN 
    ocValue = BongHode.EAv.
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_TTId:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN
  DO: 
    FIND FIRST BongLinje NO-LOCK WHERE 
      BongLinje.b_id = BongHode.b_id AND 
      BongLinje.TTId >= 96 AND 
      BongLinje.TTId <= 97 NO-ERROR.
    IF AVAILABLE BongLinje THEN 
      ocValue = (IF BongLinje.TTId = 96 THEN 'INNLogging' ELSE IF BongLinje.TTId = 97 THEN 'UTLogging' ELSE 'Ukjent!').
    ELSE DO:
      FIND FIRST BongLinje NO-LOCK WHERE 
        BongLinje.b_id = BongHode.b_id NO-ERROR. 
      IF AVAILABLE BongLinje THEN 
        ocValue = STRING(BongLinje.TTId).
      ELSE 
        ocValue = ''.
    END.
  END.
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_IntTTId:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN
  DO: 
    FIND FIRST BongLinje NO-LOCK WHERE 
      BongLinje.b_id = BongHode.b_id AND 
      BongLinje.TTId >= 96 AND 
      BongLinje.TTId <= 97 NO-ERROR.
    IF AVAILABLE BongLinje THEN 
      ocValue = STRING(BongLinje.TTId).
    ELSE DO:
      FIND FIRST BongLinje NO-LOCK WHERE 
        BongLinje.b_id = BongHode.b_id NO-ERROR. 
      IF AVAILABLE BongLinje THEN 
        ocValue = STRING(BongLinje.TTId).
      ELSE 
        ocValue = ''.
    END.
  END.
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE Bonglinje_RefTekst:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN
  DO: 
    FIND FIRST BongLinje NO-LOCK WHERE 
      BongLinje.b_id = BongHode.b_id NO-ERROR.
    IF AVAILABLE BongLinje THEN 
      ocValue = BongLinje.RefTekst .
  END.
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_ODatoTid:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN 
    ocValue = STRING(DATETIME(BongHode.ODato, BongHode.OTid * 1000), "99/99-9999 HH:MM:SS").
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_EDatoTid:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN 
    ocValue = STRING(DATETIME(BongHode.EDato, BongHode.ETid * 1000), "99/99-9999 HH:MM:SS").
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_AnsattNr:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN
    DO:
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.
      IF AVAILABLE Selger THEN 
        ocValue = Selger.AnsattNr.
    END. 
  ELSE 
    ocValue = ''.
    
END PROCEDURE.

PROCEDURE bongHode_AnsNavn:
  DEF INPUT  PARAM irBongHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  FIND FIRST BongHode NO-LOCK WHERE 
    ROWID(BongHode) = irBongHode NO-ERROR.
    
  IF AVAILABLE BongHode THEN
    DO:
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.
      IF AVAILABLE Selger THEN 
        ocValue = Selger.ForNavn + ' ' + Selger.Navn.
    END. 
  ELSE 
    ocValue = ''.
    
END PROCEDURE.









