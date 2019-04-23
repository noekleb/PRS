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
