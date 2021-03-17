  DEFINE VARIABLE iX AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
  IF ERROR-STATUS:ERROR THEN 
    DO:
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            cTekst = ctekst + 
                     (IF cTekst <> '' THEN CHR(10) ELSE '') +         
                     STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix). 
        END.
    END.
  
  MESSAGE 'TEST-1' iPksdlbutNr ERROR-STATUS:ERROR SKIP
  ctekst  
  VIEW-AS ALERT-BOX.
  
