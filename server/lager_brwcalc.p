DEFINE VARIABLE iCl AS INT NO-UNDO.
/*DEFINE VARIABLE iUtvidetStatus AS INTEGER NO-UNDO.*/
DEFINE VARIABLE obOk AS LOG NO-UNDO.

/*{syspara.i 19 9 4 iUtvidetStatus INT}*/
/*{syspara.i 5 1 1 iCl INT}.           */
/*FIND Butiker NO-LOCK WHERE           */
/*  Butiker.Butik = iCl NO-ERROR.      */

PROCEDURE Lager_ButNr:
  DEF INPUT  PARAM irLager  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND Lager NO-LOCK
    WHERE ROWID(Lager) = irLager
    NO-ERROR.
  IF AVAILABLE Lager THEN
  DO:
    IF Lager.Butik = 999999 THEN 
      ocValue = 'Total'.
    ELSE 
      ocValue = STRING(Lager.Butik). 
  END.  
  ELSE ocValue = 'Ukjent'.
END PROCEDURE. 

